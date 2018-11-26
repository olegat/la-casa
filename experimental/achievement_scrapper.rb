require 'open-uri'
require 'nokogiri'
require 'json'


#------------------------------------------------------------------------------
#   General-purpose Yeti Metadata dictionary factories
#------------------------------------------------------------------------------

# Create Yeti achievements details JSON dict
# TODO - might need join_details_dict for multiple languages
def make_details_dict(lang, name, desc)
  return {
    name: {
      localized_text: [ { language: lang, text: name } ],
    },
    description: {
      localized_text: [ { language: lang, text: desc } ],
    }
  }
end

# Create Yeti achievement with a single localization.
def make_basic_details_dict(id, lang, name, desc, pageURL, imageSrc)
  imageURL = URI::join( pageURL, imageSrc )
  details  = make_details_dict( lang, name, desc )
  return {
    "achievement_identifier" => { "achievement_id" => id },
    "type"                   => "STANDARD",
    "unlocked_details"       => details.clone,
    "locked_details"         => details.clone,
    "remote_image_url"       => imageURL
  }
end



#------------------------------------------------------------------------------
#   Website Scrapping
#------------------------------------------------------------------------------
def scrap_xboxachievements_com(pageURL)
  page = Nokogiri::HTML(open(pageURL))

  # Scrap the main table with the achievements
  dataTable = page.xpath('//*[@id="dataTable"]')

  # Scrap the <td> tags that we need
  tds  = []
  ac1s = dataTable.xpath('.//tr//td[@class="ac1"]')
  ac2s = dataTable.xpath('.//tr//td[@class="ac2"]') # equal to size of ac1s
  ac3s = dataTable.xpath('.//tr//td[@class="ac3"]') # double the size of ac1s
  # TODO check assumption ac1s.length() == ac2s.length() and ac1s.length()*2 == ac3s.length()
  for i in 1..ac1s.length() do
    tds << {
      :ac1 => ac1s[i],  # Picture
      :ac2 => ac2s[i],  # Name
      :ac3 => ac3s[i*2] # Descriptions (note: i*2+1 has unneeded extra icons).
    }
  end

  # Parse the <td> tags
  achvs = []
  for i in 0..tds.length() do
    begin
      # Parse
      name = tds[i][:ac2].text
      desc = tds[i][:ac3].text
      src  = tds[i][:ac1].xpath('.//img').attr('src')

      # Build dict/JsonObj
      achvs <<
        make_basic_details_dict("#{i+1}", "en", name, desc, pageURL, src)
    rescue
      next # Ignore tags that don't respect expected format
    end
  end

  return achvs
end


def scrap_steamcommunity_com(pageURL)
  page = Nokogiri::HTML(open(pageURL))

  result = []
  id = 1
  achieveRows = page.xpath('//div[@class="achieveRow "]')
  for row in achieveRows do
    # Scrap the tags
    h3  = row.xpath('.//div[@class="achieveTxt"]//h3')
    h5  = row.xpath('.//div[@class="achieveTxt"]//h5')
    img = row.xpath('.//div[@class="achieveImgHolder"]//img')

    # Parse tags
    src  = img[0].attr('src')
    name = h3.text
    desc = h5.text

    # Create achievement dict
    result <<
      make_basic_details_dict("#{id}", "en", name, desc, pageURL, src)
    id = id + 1
  end
  return result
end


def scrapWebpage(url)
  if url.include? "xboxachievements.com" then
    return scrap_xboxachievements_com(url)

  elsif url.include? "steamcommunity.com" then
    return scrap_steamcommunity_com(url)
  end

  raise "Unsupport website #{url}"
end



#------------------------------------------------------------------------------
#   Yeti Metadata Generation
#------------------------------------------------------------------------------
#
# Write Ruby achievement dict into content.json
def writeJSONData(achvDict, outDir)
  FileUtils.mkdir_p( outDir )
  File.open("#{outDir}/content.json", "w") do |file|
    file.write(JSON.pretty_generate(achvDict))
  end
end

# Download images from remote_image_url, and return a dict identical to
# remoteAchvs but using local file paths
#
def downloadImageFiles(remoteAchvs, outDir)
  localAchvs = []
  subDir     = "assets/achievements"
  FileUtils.mkdir_p( "#{outDir}/#{subDir}" )

  for a in remoteAchvs do
    name = a["achievement_identifier"]["achievement_id"]
    url  = a["remote_image_url"].to_s
    ext  = File.extname(url)

    # Download the file
    downloadSubpath = "#{subDir}/achievements_icon_#{name}#{ext}"
    open("#{outDir}/#{downloadSubpath}", 'wb') do |file|
      file << open( url ).read
      file.close()
    end

    # Copy 'a' but exclude :remote_image_url and add :image_icon
    localAchvs << {
      "achievement_identifier" => a["achievement_identifier"].clone,
      "type"                   => a["type"].clone,
      "unlocked_details"       => a["unlocked_details"].clone,
      "locked_details"         => a["locked_details"].clone,
      "image_icon" => { locators: [ { path: downloadSubpath } ] }
    }

  end

  return localAchvs
end

# Generate all Yeti metadata
def generateYetiMetadata(remoteAchievements, outDir)
  # Download images.
  # writes image files to assets/achievements/
  # returns array of dicts with :image_icon:locators
  localAchievements = downloadImageFiles( remoteAchievements, outDir )

  # Write assets/achievements/content.json
  writeJSONData( {"achievements" => localAchievements}, outDir )
end



#------------------------------------------------------------------------------
#   Main Entry
#------------------------------------------------------------------------------
def main(pageURL, outDir)
  # Scrap.
  # returns array of dicts with :remote_image_url
  metaData = scrapWebpage( pageURL )

  # Write to files.
  generateYetiMetadata( metaData, outDir )

  print "Outputted data to \"#{outDir}/\"\n"
end


# Program parameters. These should be args / opts
#
#   Shadow of Mordor:
#     https://www.xboxachievements.com/game/middle-earth-shadow-of-mordor/achievements/
#     https://steamcommunity.com/stats/241930/achievements/
#
#   Mad Max:
#     https://www.xboxachievements.com/game/mad-max-xbox-one/achievements/
#     https://steamcommunity.com/stats/234140/achievements/
#
main(
  'https://www.xboxachievements.com/game/middle-earth-shadow-of-mordor/achievements/',
  "out"
)
