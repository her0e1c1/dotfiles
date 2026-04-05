// spec: specs/plan.md
// seed: seed.spec.ts

import { test, expect } from '@playwright/test';

test.describe('TodoMVC', () => {
  test('Filter todos by Active and Completed', async ({ page }) => {
    // Navigate to TodoMVC app
    await page.goto('https://demo.playwright.dev/todomvc');

    // Add first todo 'Buy milk'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Buy milk');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Add second todo 'Walk the dog'
    await page.getByRole('textbox', { name: 'What needs to be done?' }).fill('Walk the dog');
    await page.getByRole('textbox', { name: 'What needs to be done?' }).press('Enter');

    // Mark 'Buy milk' as complete by clicking its Toggle Todo checkbox
    await page.getByRole('listitem').filter({ hasText: 'Buy milk' }).getByLabel('Toggle Todo').click();

    // Click the 'Active' filter to show only active todos
    await page.getByRole('link', { name: 'Active' }).click();

    // Verify 'Walk the dog' is visible in Active filter
    await expect(page.getByText('Walk the dog')).toBeVisible();

    // Click the 'Completed' filter to show only completed todos
    await page.getByRole('link', { name: 'Completed' }).click();

    // Verify 'Buy milk' is visible in Completed filter
    await expect(page.getByText('Buy milk')).toBeVisible();
  });
});
