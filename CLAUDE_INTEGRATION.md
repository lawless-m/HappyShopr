# Claude AI Integration Guide

This guide explains how to integrate HappyShopr with Claude AI for conversational shopping list management.

## Overview

Claude can manage your shopping list through natural conversation by making API calls to HappyShopr. The integration supports:

- Adding items from recipe discussions
- Tracking recipe context across conversations
- Managing items (complete, delete, update)
- Viewing lists and items
- Recipe grouping and progress tracking

## Setup

### 1. Deploy HappyShopr

Follow the [DEPLOYMENT.md](DEPLOYMENT.md) guide to deploy HappyShopr on your server.

### 2. Get Your API Credentials

After deployment, note:
- **API URL:** `http://your-server:8080` (or your domain with HTTPS)
- **API Key:** From your `config/sys.config` file
- **List ID:** Create a list via the web UI or API

### 3. Add to Claude Project Knowledge

In your Claude project, add this to the project knowledge:

```markdown
# HappyShopr Shopping List Integration

## API Configuration

Base URL: http://your-server:8080/api/v1
API Key: your-secure-api-key-here
Primary List ID: your-list-id-here

## Authentication

All API requests require Bearer token authentication:
```
Authorization: Bearer your-secure-api-key-here
```

## Common Operations

### Add Items to Shopping List

When user mentions ingredients or items to buy:

```python
POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Item Name", "quantity": "amount"}
  ]
}
```

### Add Items from Recipe

When user discusses a recipe, generate a UUID and tag items:

```python
import uuid
recipe_id = str(uuid.uuid4())

POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Spaghetti", "quantity": "500g"},
    {"name": "Bacon", "quantity": "200g"}
  ],
  "recipe_id": recipe_id,
  "recipe_name": "Spaghetti Carbonara"
}
```

### View Shopping List

```python
GET /api/v1/lists/{list_id}
```

### Remove Item

```python
DELETE /api/v1/lists/{list_id}/items/{item_id}
```

### Mark Item as Completed

```python
POST /api/v1/lists/{list_id}/items/{item_id}/complete
{"completed": true}
```

## Context Tracking

Maintain recipe context during conversation:

1. When user mentions a recipe, generate and store recipe_id
2. For follow-up additions, use the same recipe_id
3. Track recipe_id in conversation memory

Example:
- User: "I'm making Chicken Curry tonight"
- Claude: *Generates recipe_id: abc-123*
- User: "Add the ingredients"
- Claude: *Uses recipe_id: abc-123 for all items*
- User: "Also add naan bread"
- Claude: *Uses same recipe_id: abc-123*

## Example Conversations

### Example 1: Recipe from Discussion

**User:** I'm making Spaghetti Carbonara tonight. The recipe calls for:
- 400g spaghetti
- 200g bacon
- 4 eggs
- 100g parmesan
- Black pepper

**Claude:**
```
I'll add these ingredients to your shopping list under "Spaghetti Carbonara".

*Generates recipe_id: 550e8400-e29b-41d4-a716-446655440000*
*Calls API:*

POST /api/v1/lists/your-list-id/items
{
  "items": [
    {"name": "Spaghetti", "quantity": "400g"},
    {"name": "Bacon", "quantity": "200g"},
    {"name": "Eggs", "quantity": "4"},
    {"name": "Parmesan", "quantity": "100g"},
    {"name": "Black pepper"}
  ],
  "recipe_id": "550e8400-e29b-41d4-a716-446655440000",
  "recipe_name": "Spaghetti Carbonara"
}

✅ Added 5 items to your shopping list under "Spaghetti Carbonara"
```

**User:** Also add garlic

**Claude:**
```
*Uses stored recipe_id: 550e8400-e29b-41d4-a716-446655440000*

POST /api/v1/lists/your-list-id/items
{
  "items": [{"name": "Garlic"}],
  "recipe_id": "550e8400-e29b-41d4-a716-446655440000",
  "recipe_name": "Spaghetti Carbonara"
}

✅ Added garlic to your Spaghetti Carbonara ingredients
```

### Example 2: General Items

**User:** Add milk and bread to my shopping list

**Claude:**
```
*No recipe context, adds as general items*

POST /api/v1/lists/your-list-id/items
{
  "items": [
    {"name": "Milk"},
    {"name": "Bread"}
  ]
}

✅ Added milk and bread to your shopping list
```

### Example 3: View List

**User:** What's on my shopping list?

**Claude:**
```
GET /api/v1/lists/your-list-id

Your shopping list:

**Spaghetti Carbonara** [2/5]
○ Spaghetti 400g
○ Bacon 200g
✓ Eggs 4
✓ Parmesan 100g
○ Black pepper

**General Items**
○ Milk
○ Bread

You have 7 items total, 2 completed.
```

### Example 4: Complete Items

**User:** I bought the eggs and parmesan

**Claude:**
```
*Finds items by name and marks as completed*

POST /api/v1/lists/your-list-id/items/{egg-item-id}/complete
{"completed": true}

POST /api/v1/lists/your-list-id/items/{parmesan-item-id}/complete
{"completed": true}

✅ Marked eggs and parmesan as completed
```

## Advanced Features

### Recipe Progress Tracking

Get summary of recipes in the list:

```python
GET /api/v1/lists/{list_id}/recipes

Response:
{
  "recipes": [
    {
      "recipe_id": "550e8400-e29b-41d4-a716-446655440000",
      "recipe_name": "Spaghetti Carbonara",
      "total_items": 5,
      "completed_items": 2,
      "all_completed": false
    }
  ]
}
```

### Get Items for Specific Recipe

```python
GET /api/v1/lists/{list_id}/recipes/{recipe_id}/items
```

### Clear Completed Items

After shopping, permanently delete completed items:

```python
DELETE /api/v1/lists/{list_id}/items/completed
```

## Best Practices

### 1. Context Awareness

- Detect recipe discussions and automatically generate recipe_id
- Maintain recipe_id in conversation memory
- Use descriptive recipe names

### 2. Natural Language Processing

Parse quantities from user input:
- "2 litres of milk" → `{name: "Milk", quantity: "2 litres"}`
- "Bread" → `{name: "Bread"}`
- "500g spaghetti" → `{name: "Spaghetti", quantity: "500g"}`

### 3. Confirmations

Always confirm actions:
- ✅ "Added 5 items to your shopping list"
- ✅ "Removed bacon from your list"
- ✅ "Marked eggs as completed"

### 4. Error Handling

Handle API errors gracefully:
```python
try:
    # API call
except Exception as e:
    "I couldn't add those items. Please check your connection or API configuration."
```

### 5. Smart Suggestions

- Suggest completing items: "Have you bought the milk yet?"
- Remind about recipes: "You still need 3 items for Chicken Curry"
- Offer to clear completed: "You have 5 completed items. Would you like me to clear them?"

## Web UI Integration

Users can also manage their list via the web interface at:
```
http://your-server:8080
```

The web UI provides:
- Recipe-grouped view
- Progress tracking
- Mark items complete/incomplete
- Toggle required/not required
- Clear completed items

## Troubleshooting

### Authentication Errors

If you get 401 Unauthorized:
- Check API key is correct
- Verify Bearer token format: `Authorization: Bearer <key>`
- Ensure key matches `config/sys.config`

### List Not Found (404)

- Verify list_id is correct
- List may have been deleted
- Create a new list via API or web UI

### Connection Errors

- Check server is running: `curl http://your-server:8080/health`
- Verify firewall allows port 8080
- Check network connectivity

## API Reference

For complete API documentation, see [README.md](README.md#api-reference)

## Example Implementation

Here's a simple Claude integration example:

```python
import requests
import uuid

class HappyShoprClient:
    def __init__(self, base_url, api_key, list_id):
        self.base_url = base_url.rstrip('/')
        self.api_key = api_key
        self.list_id = list_id
        self.headers = {
            'Authorization': f'Bearer {api_key}',
            'Content-Type': 'application/json'
        }

    def add_items(self, items, recipe_name=None):
        url = f'{self.base_url}/api/v1/lists/{self.list_id}/items'

        data = {'items': items}

        if recipe_name:
            data['recipe_id'] = str(uuid.uuid4())
            data['recipe_name'] = recipe_name

        response = requests.post(url, json=data, headers=self.headers)
        response.raise_for_status()
        return response.json()

    def get_list(self):
        url = f'{self.base_url}/api/v1/lists/{self.list_id}'
        response = requests.get(url, headers=self.headers)
        response.raise_for_status()
        return response.json()

    def mark_completed(self, item_id, completed=True):
        url = f'{self.base_url}/api/v1/lists/{self.list_id}/items/{item_id}/complete'
        response = requests.post(url, json={'completed': completed}, headers=self.headers)
        response.raise_for_status()
        return response.json()

# Usage
client = HappyShoprClient(
    'http://your-server:8080',
    'your-api-key',
    'your-list-id'
)

# Add recipe items
client.add_items([
    {'name': 'Chicken', 'quantity': '500g'},
    {'name': 'Curry paste', 'quantity': '2 tbsp'}
], recipe_name='Chicken Curry')

# View list
list_data = client.get_list()
print(f"You have {len(list_data['items'])} items")

# Mark item complete
client.mark_completed(item_id, completed=True)
```

## Support

For integration issues:
- API Documentation: [README.md](README.md)
- Deployment Guide: [DEPLOYMENT.md](DEPLOYMENT.md)
- GitHub Issues: <your-repo-url>/issues
