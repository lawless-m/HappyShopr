# HappyShopr - Key Behavior Clarifications

## Important Distinctions

### 1. Completed Items Are NOT Deleted

**The Behavior:**
- When you mark an item as "completed" (acquired/purchased), it remains in the list
- Completed items are displayed with strikethrough text
- They remain visible unless you choose to hide them with a filter
- The item's `completed` field is set to `true`, but the record persists in the database

**Why This Matters:**
- You can see what you've already bought during your shopping trip
- You can review past purchases
- You can "uncomplete" an item if you made a mistake
- History is preserved

**Optional Permanent Deletion:**
- There's a separate "Clear Completed" button/endpoint
- This **permanently deletes** completed items from the database
- This is optional - you can leave completed items forever if you want
- Useful for cleaning up after a shopping trip is done

**Visual Examples:**

Before shopping:
```
○ Milk 2 litres
○ Bread
○ Eggs
```

After marking Bread as completed:
```
○ Milk 2 litres
✓ Bread  (strikethrough, still visible)
○ Eggs
```

After clicking "Clear Completed":
```
○ Milk 2 litres
○ Eggs
(Bread is now permanently deleted)
```

---

### 2. Required Field for Recipe Management

**The Feature:**
- Each item has a `required` boolean field (defaults to `true`)
- When viewing a recipe, you can toggle whether an ingredient is "required"
- Marking an item as "not required" means you don't need it for this recipe

**Use Cases:**

**Example 1: Already Have It**
```
Recipe: Spaghetti Carbonara
- Spaghetti ✓ Required
- Bacon ✓ Required  
- Eggs ✓ Required
- Parmesan ✓ Required
- Black pepper → [Mark as Not Required] "I already have this at home"
```

**Example 2: Optional Ingredient**
```
Recipe: Thai Green Curry
- Chicken breast ✓ Required
- Coconut milk ✓ Required
- Curry paste ✓ Required
- Fish sauce ✓ Required
- Thai basil → [Mark as Not Required] "I'll skip this garnish"
```

**Example 3: Substitution**
```
Recipe: Lasagne
- Lasagne sheets ✓ Required
- Beef mince ✓ Required
- Tomato sauce ✓ Required
- Béchamel sauce → [Mark as Not Required] "I'm using ricotta instead"
```

**How It Works in the UI:**

Recipe view before toggling:
```
┌─────────────────────────────────────┐
│  Spaghetti Carbonara          [4/5] │
│                                     │
│  Required:                          │
│  ○ Spaghetti 500g      [Not Req]   │
│  ✓ Bacon 200g          [Not Req]   │
│  ○ Eggs (4)            [Not Req]   │
│  ✓ Parmesan            [Not Req]   │
│  ○ Black pepper        [Not Req]   │
└─────────────────────────────────────┘
```

After clicking [Not Req] on Black pepper:
```
┌─────────────────────────────────────┐
│  Spaghetti Carbonara          [4/4] │
│                                     │
│  Required:                          │
│  ○ Spaghetti 500g      [Not Req]   │
│  ✓ Bacon 200g          [Not Req]   │
│  ○ Eggs (4)            [Not Req]   │
│  ✓ Parmesan            [Not Req]   │
│                                     │
│  Not Required:                      │
│  ○ Black pepper        [Required]   │
│    (dimmed/grayed out)              │
└─────────────────────────────────────┘
```

Notice:
- Progress counter changed from [4/5] to [4/4]
- Black pepper moved to "Not Required" section
- You can still complete not-required items if you want
- You can toggle it back to required with [Required] button

---

## Database Schema

```erlang
-record(shopping_item, {
    id,
    list_id,
    name,
    quantity,
    notes,
    category,
    recipe_id,
    recipe_name,
    required,      % NEW: Is this item needed? (default: true)
    completed,     % Is this item acquired? (NOT deleted, just marked)
    created_at,
    added_by
}).
```

---

## API Endpoints

### Mark Item as Completed (NOT deleted)
```
POST /api/v1/lists/:list_id/items/:item_id/complete
Body: {"completed": true}
```
Item remains in database with `completed: true`

### Mark Item as Not Completed
```
POST /api/v1/lists/:list_id/items/:item_id/complete
Body: {"completed": false}
```
Unmarks the item (e.g., you put it back on the shelf)

### Toggle Required Status
```
POST /api/v1/lists/:list_id/items/:item_id/required
Body: {"required": false}
```
Item remains but is marked as "not needed"

### Permanently Delete Completed Items
```
DELETE /api/v1/lists/:list_id/items/completed
```
DESTRUCTIVE: Actually deletes records from database

---

## User Experience Examples

### Scenario 1: Shopping Trip

**Start of trip:**
```
Shopping List (0/10 completed)
○ Milk
○ Bread  
○ Eggs
○ Butter
○ Cheese
○ Chicken
○ Rice
○ Pasta
○ Tomatoes
○ Onions
```

**During shopping (items marked as acquired):**
```
Shopping List (4/10 completed)
✓ Milk       (strikethrough)
✓ Bread      (strikethrough)
○ Eggs
✓ Butter     (strikethrough)
○ Cheese
○ Chicken
○ Rice
✓ Pasta      (strikethrough)
○ Tomatoes
○ Onions
```

**End of trip (optionally clear completed):**
```
Click [Clear Completed (4)] button

Shopping List (0/6 completed)
○ Eggs
○ Cheese
○ Chicken
○ Rice
○ Tomatoes
○ Onions
(The 4 completed items are now permanently deleted)
```

### Scenario 2: Recipe Planning

**Claude adds recipe ingredients:**
```
You: "Add ingredients for Chicken Curry"

Claude adds:
- Chicken breast (required: true)
- Coconut milk (required: true)
- Curry paste (required: true)
- Fish sauce (required: true)
- Thai basil (required: true)
- Lime (required: true)
```

**You review the recipe:**
```
Open recipe detail view
Click [Not Required] on:
- Thai basil (already have it)
- Lime (skipping this)

Progress now shows: [0/4] instead of [0/6]
```

**While shopping:**
```
Chicken Curry [2/4]

Required:
✓ Chicken breast
✓ Coconut milk
○ Curry paste
○ Fish sauce

Not Required:
○ Thai basil
○ Lime
```

---

## Implementation Notes for Claude Code

### Default Values
```erlang
% When creating a new item
#shopping_item{
    id = generate_uuid(),
    name = Name,
    quantity = Quantity,
    required = true,      % Always default to true
    completed = false,    % Always default to false
    created_at = now()
}
```

### Completion Counting
```erlang
% Count only required items for progress
count_progress(Items) ->
    RequiredItems = [I || I <- Items, I#shopping_item.required =:= true],
    CompletedItems = [I || I <- RequiredItems, I#shopping_item.completed =:= true],
    {length(CompletedItems), length(RequiredItems)}.
```

### CSS Classes for Frontend
```css
.item-completed {
    text-decoration: line-through;
    opacity: 0.6;
}

.item-not-required {
    opacity: 0.4;
    font-style: italic;
}

.item-not-required.item-completed {
    text-decoration: line-through;
    opacity: 0.3;
}
```

---

## Summary

✅ **Completed ≠ Deleted**
- Completed items remain in database
- Displayed with strikethrough
- Can be uncompleted
- Can be permanently deleted later (optional)

✅ **Required Field**
- Toggle whether recipe ingredients are needed
- Progress counts only required items
- Not-required items are dimmed but still accessible
- Can be toggled back to required anytime

✅ **User Control**
- Users choose when to permanently delete items
- Users choose which recipe ingredients they actually need
- Everything is reversible except permanent deletion
- Clear visual feedback for all states
