import { expect, test } from "@playwright/test";

// Firefox avoids Google's headless Chromium bot interstitial in this containerized test flow.
test.use({ browserName: "firefox" });

test("searches today's weather from google.com", async ({ page }) => {
  const query = "今日の天気";

  await page.goto("https://www.google.com/", { waitUntil: "domcontentloaded" });

  const consentButton = page
    .getByRole("button")
    .filter({
      hasText:
        /Accept all|I agree|同意する|すべて受け入れる|全て受け入れる|Alle akzeptieren/,
    })
    .first();

  if (await consentButton.isVisible().catch(() => false)) {
    await consentButton.click();
  }

  const searchBox = page.locator('textarea[name="q"], input[name="q"]').first();

  await expect(searchBox).toBeVisible();
  await searchBox.fill(query);
  await searchBox.press("Enter");

  await expect(page).toHaveURL(/google\.[^/]+\/search/);
});
