# TodoMVC Test Plan

## Application Overview

Test plan for the TodoMVC demo application at https://demo.playwright.dev/todomvc/. The plan covers the primary end-user flows for a fresh, blank todo list: viewing the empty state, creating todos, validating invalid input, completing and filtering items, editing and deleting items, clearing completed entries, and confirming persistence across reloads. Each scenario is independent and assumes a fresh browser state with no existing todos.

## Test Scenarios

### 1. Initial state and todo creation

**Seed:** `tests/seed.spec.ts`

#### 1.1. Display the empty-state landing view

**File:** `tests/todomvc/empty-state.spec.ts`

**Steps:**
  1. -
    - expect: Open the TodoMVC app in a fresh state with no existing todos.
    - expect: The page title should indicate TodoMVC.
    - expect: The header should display the 'todos' title.
    - expect: The new-todo input with placeholder or accessible name 'What needs to be done?' should be visible and focused.
    - expect: No todo list, footer counter, filter links, toggle-all control, or clear-completed button should be shown.
    - expect: Failure condition: any pre-existing todo appears or task-management controls are visible before the first item is created.

#### 1.2. Create a single todo item

**File:** `tests/todomvc/create-single-todo.spec.ts`

**Steps:**
  1. Type a valid todo title such as 'Buy milk' into the new-todo input and press Enter.
    - expect: A new todo item labeled 'Buy milk' should be added to the list.
    - expect: The input should clear after submission and remain ready for another entry.
    - expect: The items-left counter should show '1 item left'.
    - expect: The toggle-all control and footer with filter links should become visible.
    - expect: Failure condition: the item is not created, the text changes unexpectedly, or the input retains the submitted value.

#### 1.3. Create multiple todos in entry order

**File:** `tests/todomvc/create-multiple-todos.spec.ts`

**Steps:**
  1. Add three todos one at a time, for example 'Buy milk', 'Read book', and 'Walk dog'.
    - expect: All three todos should appear in the list in the same order they were entered.
    - expect: Each item should start in the active, unchecked state.
    - expect: The counter should show '3 items left'.
    - expect: Failure condition: items are reordered, merged, duplicated, or created as completed.

#### 1.4. Reject empty and whitespace-only submissions and trim valid text

**File:** `tests/todomvc/validate-new-todo-input.spec.ts`

**Steps:**
  1. Press Enter with the new-todo input empty.
    - expect: No todo should be created.
    - expect: The page should remain in the empty state.
    - expect: Failure condition: a blank todo item is added.
  2. Enter only spaces in the new-todo input and press Enter.
    - expect: No todo should be created.
    - expect: Failure condition: a whitespace-only todo item is added.
  3. Enter a todo with leading and trailing spaces, such as '   Learn Playwright   ', and press Enter.
    - expect: One todo should be created.
    - expect: The displayed label should be trimmed to 'Learn Playwright'.
    - expect: The counter should show '1 item left'.
    - expect: Failure condition: extra surrounding spaces are preserved in the saved label.

### 2. Completion, filtering, and bulk actions

**Seed:** `tests/seed.spec.ts`

#### 2.1. Complete and reactivate a single todo

**File:** `tests/todomvc/toggle-single-todo.spec.ts`

**Steps:**
  1. Create two todos in a fresh state.
    - expect: Both todos should appear as active and unchecked.
    - expect: The counter should show '2 items left'.
  2. Check the toggle checkbox for the first todo.
    - expect: The selected todo should display as completed.
    - expect: The items-left counter should decrease to '1 item left'.
    - expect: A 'Clear completed' button should appear.
    - expect: Failure condition: the wrong item changes state or the counter remains unchanged.
  3. Uncheck the same todo.
    - expect: The todo should return to the active state.
    - expect: The counter should return to '2 items left'.
    - expect: If no items remain completed, the 'Clear completed' button should disappear.
    - expect: Failure condition: the item stays completed or the completed-action controls remain stale.

#### 2.2. Filter todos by All, Active, and Completed

**File:** `tests/todomvc/filter-todos.spec.ts`

**Steps:**
  1. Create three todos and mark exactly one of them as completed.
    - expect: The list should contain a mix of active and completed items.
    - expect: The counter should reflect only active items.
  2. Select the 'Active' filter.
    - expect: Only active todos should be shown.
    - expect: Completed items should be hidden from the current view.
    - expect: The URL hash should update to '#/active'.
    - expect: The Active filter should be visibly selected.
    - expect: Failure condition: completed items remain visible in the filtered view.
  3. Select the 'Completed' filter.
    - expect: Only completed todos should be shown.
    - expect: Active items should be hidden from the current view.
    - expect: The URL hash should update to '#/completed'.
    - expect: The Completed filter should be visibly selected.
    - expect: Failure condition: active items remain visible in the filtered view.
  4. Select the 'All' filter.
    - expect: All todos should be shown again regardless of status.
    - expect: The URL hash should return to '#/'.
    - expect: The All filter should be visibly selected.
    - expect: Failure condition: the full list is not restored.

#### 2.3. Mark all todos complete in one action

**File:** `tests/todomvc/toggle-all.spec.ts`

**Steps:**
  1. Create at least two active todos.
    - expect: All todos should start unchecked.
    - expect: The items-left counter should equal the total number of todos.
  2. Use the 'Mark all as complete' control.
    - expect: Every todo should become completed.
    - expect: The items-left counter should update to '0 items left'.
    - expect: The toggle-all control should show the checked state.
    - expect: The 'Clear completed' button should appear.
    - expect: Failure condition: any todo remains active after the bulk action.

#### 2.4. Clear completed items while keeping active items

**File:** `tests/todomvc/clear-completed.spec.ts`

**Steps:**
  1. Create multiple todos and mark only a subset as completed.
    - expect: Both active and completed items should be present.
    - expect: The 'Clear completed' button should be visible.
  2. Click 'Clear completed'.
    - expect: All completed todos should be removed from the data set and UI.
    - expect: Any active todos should remain in the list unchanged and in order.
    - expect: The items-left counter should reflect the remaining active items.
    - expect: If no completed items remain, the 'Clear completed' button should disappear.
    - expect: Failure condition: active items are removed or completed items remain visible afterward.

### 3. Editing, deletion, and persistence

**Seed:** `tests/seed.spec.ts`

#### 3.1. Edit an existing todo and save with Enter

**File:** `tests/todomvc/edit-todo-save.spec.ts`

**Steps:**
  1. Create a todo such as 'Buy milk'.
    - expect: The new todo should appear in the list.
  2. Double-click the todo label to enter edit mode.
    - expect: An inline edit textbox should appear containing the current todo text.
    - expect: The todo should enter editing mode.
    - expect: Failure condition: double-clicking does not expose the edit field.
  3. Replace the text with 'Buy oat milk' and press Enter.
    - expect: The edit mode should close.
    - expect: The todo label should update to 'Buy oat milk'.
    - expect: The item should remain in the list with its status preserved.
    - expect: Failure condition: the old label remains or the item disappears unexpectedly.

#### 3.2. Cancel todo editing with Escape

**File:** `tests/todomvc/edit-todo-cancel.spec.ts`

**Steps:**
  1. Create a todo such as 'Walk dog'.
    - expect: The todo should appear in the list.
  2. Enter edit mode for the todo, change the text, and press Escape instead of Enter.
    - expect: Edit mode should close without saving the draft value.
    - expect: The original label 'Walk dog' should remain unchanged.
    - expect: Failure condition: the draft value is saved after Escape.

#### 3.3. Delete a todo by clearing its text during edit

**File:** `tests/todomvc/edit-empty-deletes.spec.ts`

**Steps:**
  1. Create two todos in a fresh state.
    - expect: Both todos should be visible.
    - expect: The counter should match two active items.
  2. Open edit mode for the first todo, remove all text, and press Enter.
    - expect: The edited todo should be deleted from the list.
    - expect: The remaining todo should stay visible and unchanged.
    - expect: The counter should decrease by one.
    - expect: Failure condition: an empty-labeled todo remains in the list.

#### 3.4. Delete a todo using the row delete control

**File:** `tests/todomvc/delete-todo.spec.ts`

**Steps:**
  1. Create at least two todos.
    - expect: Both todos should be visible in the list.
  2. Hover the first todo row if needed and click its delete control (×).
    - expect: Only the targeted todo should be removed.
    - expect: Other todos should remain in the same relative order.
    - expect: The counter should update to reflect the removal.
    - expect: Failure condition: the wrong todo is deleted or the counter is incorrect.

#### 3.5. Persist todos and completion state after reload

**File:** `tests/todomvc/persistence-reload.spec.ts`

**Steps:**
  1. Create two todos and mark them completed using individual toggles or the toggle-all control.
    - expect: Both todos should appear with completed styling.
    - expect: The counter should show '0 items left'.
  2. Reload the page.
    - expect: The same todos should still be present after reload.
    - expect: Their completed state should persist after reload.
    - expect: Relevant controls such as filters and clear-completed should remain consistent with the restored data.
    - expect: Failure condition: todos disappear, duplicate, or lose completion state after reload.

#### 3.6. Return to the empty state after removing the last remaining todo

**File:** `tests/todomvc/return-to-empty-state.spec.ts`

**Steps:**
  1. Create a single todo in a fresh state.
    - expect: The todo list and footer controls should appear.
  2. Delete the only remaining todo using either the row delete control or edit-to-empty behavior.
    - expect: The list should disappear because no todos remain.
    - expect: The footer, filters, toggle-all control, and clear-completed action should no longer be visible.
    - expect: The new-todo input should remain available for adding a new item.
    - expect: Failure condition: empty-list controls remain visible or a ghost item remains in the DOM/UI.
