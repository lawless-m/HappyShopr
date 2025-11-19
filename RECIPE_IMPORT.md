# Recipe Import from Claude Artifacts

This document explains how to import recipes from Claude artifact URLs into HappyShopr.

## Overview

The HappyShopr API now supports importing recipes from Claude artifact URLs. When you share a recipe JSON file embedded in markdown via Claude's artifact system, you can import it directly into a shopping list.

## API Endpoint

```
POST /api/v1/lists/:list_id/import-recipe
```

Also available at: `/shopr/api/v1/lists/:list_id/import-recipe`

## Authentication

Requires Bearer token authentication:

```
Authorization: Bearer <your-api-key>
```

## Request Format

```json
{
  "url": "https://claude.ai/public/artifacts/your-artifact-id"
}
```

## Response Format

### Success (201 Created)

```json
{
  "success": true,
  "recipe_id": "7f8e9a2b-4c5d-4e6f-8a9b-1c2d3e4f5a6b",
  "items_added": 13,
  "message": "Recipe imported successfully"
}
```

### Error Responses

#### 400 Bad Request - Missing URL
```json
{
  "error": "url field is required"
}
```

#### 400 Bad Request - Download Failed
```json
{
  "error": "Failed to download content from URL"
}
```

#### 400 Bad Request - Invalid Format
```json
{
  "error": "Could not parse JSON from markdown"
}
```

#### 401 Unauthorized
```json
{
  "error": "unauthorized"
}
```

#### 500 Internal Server Error
```json
{
  "error": "Failed to save recipe"
}
```

## Expected Recipe Format

The artifact should contain a markdown file with a JSON code block in the following format:

```markdown
# Recipe Title

```json
{
  "recipe_id": "optional-uuid",
  "title": "Air Fryer Red Lentil Koftas",
  "description": "Recipe description...",
  "servings": 2,
  "ingredients": [
    {
      "name": "red lentils",
      "quantity": "150g",
      "category": "pulses",
      "notes": "rinsed"
    },
    {
      "name": "onion",
      "quantity": "1 medium",
      "category": "vegetables",
      "notes": "finely chopped"
    }
  ],
  "method": [...],
  "equipment": [...],
  ...
}
```
```

## How It Works

1. **URL Download**: The server downloads the content from the provided Claude artifact URL
2. **JSON Extraction**: Extracts the JSON from the markdown code block (looks for ````json ... ```)
3. **Recipe Creation**: Creates a recipe record in the database with:
   - Recipe ID (uses the one from JSON or generates a new UUID)
   - Recipe title
   - Source URL
   - User ID
   - Timestamps
4. **Item Creation**: Converts each ingredient into a shopping list item with:
   - Name
   - Quantity (optional)
   - Notes (optional)
   - Category (optional)
   - Recipe ID and name (for grouping)
5. **Storage**: Saves everything to the Mnesia database

## Example Usage

### Using curl

```bash
# Set your API key
API_KEY="your-api-key-here"

# Get or create a shopping list
LIST_RESPONSE=$(curl -s -X POST http://localhost:8080/api/v1/lists \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"name": "Weekly Shopping"}')

LIST_ID=$(echo "$LIST_RESPONSE" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)

# Import a recipe from Claude artifact
curl -X POST "http://localhost:8080/api/v1/lists/$LIST_ID/import-recipe" \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "url": "https://claude.ai/public/artifacts/2f1f0330-96bf-4f60-8834-315cdcd8679e"
  }'
```

### Using JavaScript

```javascript
const apiKey = 'your-api-key-here';
const listId = 'your-list-id';
const artifactUrl = 'https://claude.ai/public/artifacts/your-artifact-id';

const response = await fetch(`http://localhost:8080/api/v1/lists/${listId}/import-recipe`, {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${apiKey}`,
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({ url: artifactUrl })
});

const result = await response.json();
console.log(result);
```

## Integration with Claude

When sharing a recipe with users:

1. User asks Claude to create a recipe in the HappyShopr format
2. Claude generates the recipe JSON and wraps it in a markdown code block
3. Claude publishes it as an artifact
4. User gets the artifact URL (e.g., `https://claude.ai/public/artifacts/...`)
5. User sends this URL to the HappyShopr import endpoint
6. HappyShopr downloads, parses, and imports the recipe

## Notes

- The import process is idempotent - importing the same recipe multiple times will create separate entries
- All ingredients from the recipe are added to the shopping list
- Items are marked as "required" by default
- Items inherit the recipe_id for grouping and tracking
- The original artifact URL is stored in the recipe record's `source` field
- If the recipe JSON doesn't include a `recipe_id`, one will be generated automatically

## Troubleshooting

### Recipe Not Importing

1. Check that the artifact URL is accessible
2. Verify the JSON is properly formatted inside a markdown code block
3. Ensure the `ingredients` array exists and has valid items
4. Check server logs for detailed error messages

### Missing Items

- Verify all ingredients have a `name` field (required)
- Other fields (quantity, notes, category) are optional

### Authentication Errors

- Ensure your API key is valid
- Check that the `Authorization: Bearer` header is correctly formatted
