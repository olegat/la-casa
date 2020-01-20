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

function promiseGetCaseNumberInTitle() {
  return promiseTabsQuery({'active': true, 'lastFocusedWindow': true, 'currentWindow': true})
    .then(function (tabs) {
      return tabs[0].title.split(" ", 1);
    });
}

// Old way to do get the Case Number. This doesn't work well when changing
// between tabs in SF. When you change SF Case tab, the extension copies
// the case number of the 1st case tab that was opened. To the get tool to
// copy the case number of the newest selected case tab, the whole page needs
// to  be refreshed. This is likely because SF appends additional HTML
// elements as the user opens more cases, and our tools only picks up the
// first matching element (not the currently selected element).
//
// The new way to do it is to copy the HTML Page Title instead, because
// that's a singleton and it contains the case number of the selected SF tab.
function findCaseNumberInHTML() {
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

// Old way to get the Case Number. @see findCaseNumberInHTML for more info
function promiseGetCaseNumberInHTML() {
  return new Promise(function(resolve,reject){
    chrome.tabs.executeScript(
      null,
      {
        // TODO(olegat) see if possible to inject this function permanently
        code: findCaseNumberInHTML.toString() + 'findCaseNumberInHTML()'
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
  // Old way to get the Case Number. @see findCaseNumberInHTML for more info
  // pCaseNo = promiseGetCaseNumberInHTML()
  pCaseNo = promiseGetCaseNumberInTitle()
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
