// spec: specs/plan.md
// seed: seed.spec.ts

import { test, expect } from '@playwright/test';

test.describe('TodoMVC', () => {
  test('Add new todo', async ({ page }) => {
    // Navigate to TodoMVC app
    await page.goto('https://demo.playwright.dev/todomvc');

    // Type a new todo item in the input field
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Buy groceries');

    // Press Enter to add the todo item
    await page.keyboard.press('Enter');

    // Verify the todo item 'Buy groceries' appears in the list
    await expect(page.getByText('Buy groceries')).toBeVisible();

    // Verify item count shows '1 item left'
    await expect(page.getByText('1')).toBeVisible();
  });
});
