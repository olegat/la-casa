Avoid white-box testing (reflection / introspecting). AG Charts internals
generally do not have formal invariants and therefore assumptions of internals
should be avoided. Favour black-box testing of the public AG Charts API instead
(i.e. good tests use the public API like a real end-user). Exceptions are
allowed for general-purpose utilities that implement formal mathematical
operations or standards (e.g. some internally `lerp` function or an ISO-8601
parser).

Use DAMP-not-DRY when writing tests. Clearly separate computational logic from
reproduction steps. Keeps `expect` calls in test cases as much as possible
(exceptions: initialisation).

Bad Example:
```
function checkChartState(expectation: unknown) {
  const result = chart.foobar(); // imagine this is many lines long.
  expect(result).toBe(expectation); // BAD! Should be in test()
}

// BAD! for-loop is DRY-not-DAMP
for (const t of someList) {
  test(t.name, async () => {
    // OKAY for individual tests that need custom initialisation.
    // BAD for initialisie multiple tests the same way.
    chart = await createChart();
    expect(chart).toBeDefined();

    // BAD! Too much logic in the repro-steps.
    const centerX = t.offsetX + (chart.width / 2);
    const centerY = t.offsetY + (chart.height / 2);
    await clickAction(centerX, centerY)(chart);
    await waitForChartStability(chart);

    // BAD! Test failures will highlight a line number shallower in the stack-call
    checkChartState(t.expectation);
});
```

Good Example:
```
function popChartState() {
  return chart.foobar(); // imagine this is many lines long.
}

async function clickCenter(offsetX: number, offsetY: number) {
    const centerX = offsetX + (chart.width / 2);
    const centerY = offsetY + (chart.height / 2);
    await clickAction(centerX, centerY)(chart);
    await waitForChartStability(chart);
}

describe('my collection', () => {
  beforeEach(async () => {
    chart = await createChart();
    expect(chart).toBeDefined(); // OKAY for initialisation
  });

  test('my first test', async () => {
    await clickCenter(10, 10)(chart);
    expect(popChartState()).toBe(something);
  });

  test('my other test', async () => {
    await clickCenter(20, 20)(chart);
    expect(popChartState()).toBe(somethingElse);
  });
});
```
