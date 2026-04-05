// spec: specs/plan.md
// seed: seed.spec.ts

import { test, expect } from '@playwright/test';

test.describe('TodoMVC', () => {
  test('Mark todo as complete', async ({ page }) => {
    // Navigate to TodoMVC app
    await page.goto('https://demo.playwright.dev/todomvc');

    // Type a new todo item 'Read a book'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Read a book');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Click the toggle checkbox to mark the todo as complete
    await page.getByRole('checkbox', { name: 'Toggle Todo' }).click();

    // Verify the Toggle Todo checkbox is checked (todo is completed)
    await expect(page.getByRole('checkbox', { name: 'Toggle Todo' })).toBeChecked();

    // Verify item count shows '0 items left'
    await expect(page.getByText('0 items left')).toBeVisible();
  });
});
