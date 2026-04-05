// spec: specs/plan.md
// seed: seed.spec.ts

import { test, expect } from '@playwright/test';

test.describe('TodoMVC', () => {
  test('Clear completed todos', async ({ page }) => {
    // Navigate to TodoMVC app
    await page.goto('https://demo.playwright.dev/todomvc');

    // Add first todo 'Clean the house'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Clean the house');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Add second todo 'Do laundry'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Do laundry');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Mark 'Clean the house' as complete
    await page.getByRole('listitem').filter({ hasText: 'Clean the house' }).getByLabel('Toggle Todo').click();

    // Click the 'Clear completed' button to remove completed todos
    await page.getByRole('button', { name: 'Clear completed' }).click();

    // Verify 'Do laundry' is still visible (active todo remains)
    await expect(page.getByText('Do laundry')).toBeVisible();
  });
});
