// spec: specs/plan.md
// seed: seed.spec.ts

import { test, expect } from '@playwright/test';

test.describe('TodoMVC', () => {
  test('Delete a todo', async ({ page }) => {
    // Navigate to TodoMVC app
    await page.goto('https://demo.playwright.dev/todomvc');

    // Add a new todo item 'Go for a walk'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Go for a walk');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Hover over the todo item to reveal the delete button
    await page.getByTestId('todo-item').hover();

    // Click the Delete button to remove the todo item
    await page.getByRole('button', { name: 'Delete' }).click();

    // Verify the todo input field is visible (list is empty - todo was deleted)
    await expect(page.getByRole('textbox', { name: 'What needs to be done?' })).toBeVisible();
  });
});
