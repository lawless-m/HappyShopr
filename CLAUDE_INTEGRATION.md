# HappyShopr API - Claude Integration Guide

This document contains the API configuration and usage instructions for Claude AI to manage shopping lists.

## API Configuration

**Base URL:** `http://172.233.189.66:8080/api/v1`
**API Key:** `demo-api-key-replace-in-production`
**Authentication:** Bearer token (include in all requests)

## Required Headers

```
Authorization: Bearer demo-api-key-replace-in-production
Content-Type: application/json
```

## Core Workflows

### 1. Get or Create a Shopping List

First, check for existing lists:
```
GET /api/v1/lists
```

If no lists exist or user wants a new one:
```
POST /api/v1/lists
{"name": "Shopping List"}
```

Save the `list_id` from the response for subsequent operations.

### 2. Add Items to List

When user mentions ingredients or items to buy:
```
POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Milk", "quantity": "2 liters"},
    {"name": "Eggs", "quantity": "12"}
  ]
}
```

### 3. Add Items from a Recipe

When user mentions a recipe with ingredients:
```
POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Spaghetti", "quantity": "500g"},
    {"name": "Bacon", "quantity": "200g"},
    {"name": "Eggs", "quantity": "4"}
  ],
  "recipe_id": "generated-uuid-here",
  "recipe_name": "Spaghetti Carbonara"
}
```

**Important:** Generate a UUID v4 for `recipe_id` and keep it consistent when adding more items to the same recipe.

### 4. Get List with All Items

```
GET /api/v1/lists/{list_id}
```

### 5. Mark Items as Completed

```
POST /api/v1/lists/{list_id}/items/{item_id}/complete
{"completed": true}
```

### 6. Toggle Item Required Status

```
POST /api/v1/lists/{list_id}/items/{item_id}/required
{"required": false}
```

### 7. View Recipe Progress

```
GET /api/v1/lists/{list_id}/recipes
```

## Behavior Notes

- **Completed items are NOT deleted** - they remain visible with strikethrough
- Use "Clear Completed" only when user explicitly requests permanent deletion
- Track recipe_id for each recipe to group related items
- Default all items to `required: true` unless user indicates otherwise
- When user says "I have X", mark that item as `required: false` instead of deleting it

## Example Conversation Flow

**User:** "I'm making Spaghetti Carbonara tonight"
**Claude:**
1. Generate UUID for recipe (e.g., `550e8400-e29b-41d4-a716-446655440000`)
2. Add ingredients with recipe_id and recipe_name
3. Confirm items added

**User:** "I already have eggs"
**Claude:**
1. Find the egg item in the list
2. Set `required: false` on that item (don't delete it)

**User:** "I bought the bacon"
**Claude:**
1. Find the bacon item
2. Mark `completed: true` (item stays in list with strikethrough)
