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
