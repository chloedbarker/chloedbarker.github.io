/* Run with NODE_PATH pointing to a Playwright installation and the site served on port 8000. */
const { chromium } = require('playwright');
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const root = path.resolve(__dirname, '..');
const pages = fs.readdirSync(root, {recursive: true}).filter(p => /(^|\/)index\.html$/.test(p) && !p.startsWith('.'));
(async () => {
 const browser = await chromium.launch({channel: 'chrome', headless: true});
 const page = await browser.newPage();
 const errors = [];
 page.on('pageerror', e => errors.push(e.message));
 for (const width of [390, 320, 768, 1440]) {
  await page.setViewportSize({width, height: 900});
  for (const route of pages) {
   await page.goto(`http://127.0.0.1:8000/${route}`, {waitUntil: 'load'});
   await page.emulateMedia({reducedMotion: 'reduce'});
   await page.waitForTimeout(100);
   const issues = await page.evaluate(() => ({
    overflow: document.documentElement.scrollWidth > innerWidth + 1, dimensions: [document.documentElement.scrollWidth, innerWidth],
    images: [...document.images].filter(i => new URL(i.src).origin === location.origin && (!i.complete || !i.naturalWidth)).map(i => i.src),
    main: document.querySelectorAll('main').length
   }));
   assert.equal(issues.overflow, false, `${route} overflows at ${width}`);
   assert.deepEqual(issues.images, [], `${route}: broken images`);
   assert.equal(issues.main, 1);
  }
 }
 await page.goto('http://127.0.0.1:8000/');
 // Filtering must preserve card and cover width; static skills must not react to hover.
 const cardWidths = await page.locator('[data-project-card]').evaluateAll(cards => cards.map(c => c.getBoundingClientRect().width));
 const skillCard = page.locator('.skill-card').first();
 const staticChip = page.locator('span.skill-chip').first();
 const appearance = el => { const s = getComputedStyle(el); return [s.backgroundColor, s.color, s.transform, s.boxShadow, s.borderColor]; };
 for (const target of [skillCard, staticChip]) {
  await page.mouse.move(0, 0);
  const before = await target.evaluate(appearance);
  await target.hover();
  assert.deepEqual(await target.evaluate(appearance), before, 'Static skills changed on hover');
 }
 for (const filter of ['healthcare','deployed','modeling','engineering','communication','all']) {
  await page.locator(`[data-filter="${filter}"]`).click();
  const cards = await page.locator('[data-project-card]:visible').count();
  assert.ok(cards > 0, filter);
  const widths = await page.locator('[data-project-card]').evaluateAll(cards => cards.map(c => c.hidden ? null : c.getBoundingClientRect().width));
  widths.forEach((width, i) => { if (width !== null) assert.ok(Math.abs(width - cardWidths[i]) < 1, `${filter} resized card ${i}`); });
  assert.equal(await page.locator(`[data-filter="${filter}"]`).getAttribute('aria-pressed'), 'true');
 }
 await page.locator('#projectSearch').fill('no-such-project');
 assert.equal(await page.locator('[data-project-card]:visible').count(), 0);
 assert.ok(await page.locator('#projectEmpty').isVisible());
 await page.locator('#projectSearch').fill('python healthcare');
 assert.ok(await page.locator('[data-project-card]:visible').count() > 0);
 await page.locator('#projectSearch').fill('');
 assert.equal(await page.locator('[data-project-card]:visible').count(), 6);
 await page.setViewportSize({width:1440,height:600});
 await page.locator('.hero').hover();
 await page.mouse.wheel(0, 900);
 await page.waitForTimeout(200);
 assert.ok(await page.locator('.hero').evaluate(e => e.scrollHeight <= e.clientHeight || e.scrollTop > 0));
 await page.locator('#projects').scrollIntoViewIfNeeded();
 await page.locator('.project-link-stretch').first().click();
 await page.waitForURL('**/capstone/index.html');
 await page.locator('.site-nav a').first().click();
 await page.waitForURL('**/index.html');
 await page.keyboard.press('Tab');
 await page.screenshot({path:'/tmp/portfolio-desktop.png'});
 await page.goto('http://127.0.0.1:8000/databases-sql/water-quality-project/');
 await page.locator('.map-tab').nth(1).click();
 await page.locator('#mapLoadBtn').click();
 assert.ok((await page.locator('.map-frame').getAttribute('src')).includes('fluoride'));
 await page.locator('.map-tab').nth(2).click();
 assert.ok((await page.locator('.map-frame').getAttribute('src')).includes('iodide'));
 await page.goto('http://127.0.0.1:8000/predictive-analytics/heart-disease-prediction/');
 await page.setViewportSize({width:390,height:844});
 await page.screenshot({path:'/tmp/portfolio-project-mobile.png',fullPage:true});
 assert.deepEqual(errors, []);
 await browser.close();
 console.log(`PASS: ${pages.length} pages at four widths; images, JS, filters, search, sidebar scroll, and map controls.`);
})().catch(e => { console.error(e); process.exit(1); });
