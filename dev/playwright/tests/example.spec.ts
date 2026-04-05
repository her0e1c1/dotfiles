import { expect, test } from "@playwright/test";

test("loads the TodoMVC demo page", async ({ page }) => {
  const response = await page.goto("https://demo.playwright.dev/todomvc", {
    waitUntil: "domcontentloaded",
  });

  expect(response).not.toBeNull();
  expect(response?.ok()).toBe(true);
});
