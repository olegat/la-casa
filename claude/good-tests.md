Avoid white-box testing (reflection / introspecting). AG Charts internals
generally do not have formal invariants and therefore assumptions of internals
should be avoided. Favour black-box testing of the public AG Charts API instead
(i.e. good tests use the public API like a real end-user). Exceptions are
allowed for general-purpose utilities that implement formal mathematical
operations or standards (e.g. some internally `lerp` function or an ISO-8601
parser).

Introspection is permitted (within reason) to help with black-box assertions,
but introspection itself should not be asserted.

Bad Example:
```
test('my example', async () => {
  chart = await createChart();
  expect((chart as any).ctx.someInternalThing.x).toBe(10);
});
```

Acceptable Example:
```
test('my example', async () => {
  chart = await createChart();
  await clickAtX((chart as any).ctx.someInternalThing.x);
  expect(chart.getState()).toBe(something);
});
```

Introspecting the internals through public APIs is also permitted. The criterion
is how the value was obtained, not how internal it looks. Anything delivered
through the public API — a callback argument, an event payload, a getter — may
be asserted as precisely as you like, even when the value itself is generated or
undocumented.

Acceptable Example:
```
test('should generate default ID', async () => {
  chart = await createChart(); // init 1 bar series with a click listener mock
  await clickSomething();
  expect(clickMock).toHaveBeenCalledWith(expect.objectContaining({ seriesId: 'BarSeries-1' }));
});
```

Bad Example:
``
  // Same value, but reached by introspection rather than delivered by the API.
  expect((deproxy(chart) as any).series[0].id).toBe('BarSeries-1');
``

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
