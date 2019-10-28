// -*- Mode:js; js-indent-level: 2 -*-
function promiseQuery(options){
  return new Promise(function(resolve,reject){
    chrome.tabs.query(options, resolve);
  });
}

function promiseGetURLWithoutQuery() {
  return promiseQuery({'active': true, 'lastFocusedWindow': true, 'currentWindow': true})
    .then(function (tabs) {
      return tabs[0].url.split("?", 1);
    });
}

function injected_getCaseNumber() {                                                      
  /** Salesforce Renders the HTML like.
      <div>
        <div>
  (1)     <span>Case Number</span>
        </div>
  (2)   <div>
          <span><span>00000001</span></span>
        </div>
      </div>
  */ 
  // Search for node (1)
  let xpath = '//span[contains(., "Case Number")]'
  let label = document.evaluate(xpath, document, null, XPathResult.ANY_TYPE, null);

  // Get first common ancestor of (1) and (2)
  let x = label.iterateNext().parentNode.parentNode;

  // Search for node (2)
  xpath = './/span'
  var value = document.evaluate(xpath, x, null, XPathResult.ANY_TYPE, null)
  value.iterateNext()

  return String(value.iterateNext().textContent)
}

function promiseGetCaseNumber() {
  return new Promise(function(resolve,reject){
    chrome.tabs.executeScript(
      null,
      {
        // TODO(olegat) see if possible to inject this function permanantely
        code: injected_getCaseNumber.toString() + 'injected_getCaseNumber()'
      },
      resolve)
  })
}

function copyToClipboard(text) {
  const input = document.createElement('input');
  input.style.position = 'fixed';
  input.style.opacity = 0;
  input.value = text;
  document.body.appendChild(input);
  input.select();
  document.execCommand('Copy');
  document.body.removeChild(input);
};

function onContextMenuClick() {
  pURL    = promiseGetURLWithoutQuery();
  pCaseNo = promiseGetCaseNumber()
  Promise.all([pURL,pCaseNo]).then(function(result) {
    var url    = result[0]
    var caseNo = result[1]
    copyToClipboard(caseNo +" = "+url)
  })
}

chrome.runtime.onInstalled.addListener(function() {
  console.log("hello world")
  chrome.contextMenus.create(
    {
      id:"shareCase_MenuItem",
      title:"Copy Case Link",
      documentUrlPatterns: ["https://google-stadia.lightning.force.com/*"]
    }
  )
});

chrome.contextMenus.onClicked.addListener(function(itemData) {
  if (itemData.menuItemId == "shareCase_MenuItem")
      onContextMenuClick()
});
