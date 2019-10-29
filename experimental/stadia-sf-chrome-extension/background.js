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

function promiseGetCaseNumber() {
  return new Promise(function(resolve,reject){
    chrome.tabs.executeScript(null, {code: 'findCaseNumber()'}, resolve)
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
  a.style.position = 'fixed';
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
