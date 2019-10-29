// -*- Mode:js; js-indent-level: 2 -*-
function promiseTabsQuery(options){
  return new Promise(function(resolve,reject){
    chrome.tabs.query(options, resolve);
  });
}

function promiseGetURLWithoutQuery() {
  return promiseTabsQuery({'active': true, 'lastFocusedWindow': true, 'currentWindow': true})
    .then(function (tabs) {
      return tabs[0].url.split("?", 1);
    });
}

function findCaseNumber() {
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
        // TODO(olegat) see if possible to inject this function permanently
        code: findCaseNumber.toString() + 'findCaseNumber()'
      },
      resolve)
  })
}

function copyTextToClipboard(text) {
  const input = document.createElement('input');
  input.style.position = 'fixed';
  input.style.opacity = 0;
  input.value = text;
  document.body.appendChild(input);
  input.select();
  document.execCommand('Copy');
  document.body.removeChild(input);
}

// https://stackoverflow.com/questions/23934656/javascript-copy-rich-text-contents-to-clipboard
function copyRichTextToClipboard(str) {
  function listener(e) {
    e.clipboardData.setData("text/html", str);
    e.clipboardData.setData("text/plain", str);
    e.preventDefault();
  }
  document.addEventListener("copy", listener);
  document.execCommand("copy");
  document.removeEventListener("copy", listener);
}

function copyLinkToClipboard(text, url) {
  const a = document.createElement('a');
  a.style.opacity = 0;
  a.textContent = text;
  a.setAttribute("href",url);
  document.body.appendChild(a);
  copyRichTextToClipboard(a.outerHTML);
  document.body.removeChild(a);
}

function onContextMenuClick() {
  pURL    = promiseGetURLWithoutQuery();
  pCaseNo = promiseGetCaseNumber()
  Promise.all([pURL,pCaseNo]).then(function(result) {
    var url    = result[0]
    var caseNo = result[1]
    copyLinkToClipboard(caseNo, url)
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
