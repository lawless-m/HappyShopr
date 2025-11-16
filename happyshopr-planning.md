# HappyShopr - Erlang/OTP Shopping List Service
## Planning & Architecture Document

**Version:** 1.0  
**Target Implementation:** Claude Code  
**Deployment:** Self-hosted VM (Erlang/OTP Release)

---

## 1. Project Overview

### Purpose
HappyShopr is a persistent shopping list service that enables Claude AI to manage shopping lists via API calls during conversations. The service provides REST endpoints for creating, reading, updating, and deleting shopping list items.

### Key Requirements
- Self-hosted Erlang/OTP application
- REST API with API key authentication
- Persistent storage using Mnesia
- Single user initially, but architected for multi-user expansion
- Integration with Claude via Project knowledge containing API credentials

### Success Criteria
- Claude can add/remove/view shopping list items via API
- Data persists across service restarts
- Service runs reliably on a VM
- Easy deployment and updates

---

## 2. Technology Stack

### Backend Technologies
- **Language:** Erlang/OTP 25+
- **Build Tool:** rebar3
- **HTTP Server:** Cowboy 2.x
- **Routing:** Cowboy routing
- **JSON:** jsx or jiffy for JSON parsing/encoding
- **Database:** Mnesia (distributed Erlang database)
- **Release:** relx (via rebar3)

### Frontend Technologies
- **Framework:** Vanilla JavaScript (or lightweight framework like Alpine.js/htmx)
- **Styling:** Tailwind CSS (via CDN for simplicity)
- **Build:** No build step - served as static files from Cowboy
- **Storage:** localStorage for caching, API for persistence

### Why These Choices
- **Cowboy:** Battle-tested, lightweight HTTP server, can serve static files easily
- **Mnesia:** Native Erlang DB, perfect for this use case, no external dependencies
- **relx:** Standard OTP release packaging, enables easy deployment and hot upgrades
- **Vanilla JS/Alpine:** Lightweight, no npm/webpack complexity, easy to maintain
- **Tailwind CDN:** Rapid styling without build pipeline

---

## 3. System Architecture

### OTP Application Structure

```
happyshopr/
├── apps/
│   └── happyshopr/
│       ├── src/
│       │   ├── happyshopr_app.erl          % Application behaviour
│       │   ├── happyshopr_sup.erl          % Main supervisor
│       │   ├── happyshopr_http_sup.erl     % HTTP server supervisor
│       │   ├── happyshopr_db.erl           % Mnesia interface module
│       │   ├── happyshopr_auth.erl         % API key authentication
│       │   ├── happyshopr_lists.erl        % Business logic for lists
│       │   ├── happyshopr_handler.erl      % Base HTTP handler
│       │   ├── handlers/
│       │   │   ├── health_handler.erl      % Health check endpoint
│       │   │   ├── lists_handler.erl       % List CRUD operations
│       │   │   ├── items_handler.erl       % Item operations
│       │   │   └── static_handler.erl      % Serve static files
│       │   └── happyshopr.app.src
│       ├── include/
│       │   └── happyshopr.hrl              % Record definitions
│       └── priv/
│           ├── config/
│           │   └── api_keys.config         % API keys configuration
│           └── static/
│               ├── index.html              % Main web interface
│               ├── css/
│               │   └── custom.css          % Custom styles (optional)
│               └── js/
│                   ├── app.js              % Main application logic
│                   ├── api.js              % API client
│                   └── components/
│                       ├── list-view.js    % List display component
│                       ├── item-form.js    % Add item form
│                       └── recipe-filter.js % Recipe filtering
├── config/
│   ├── sys.config                          % Runtime configuration
│   └── vm.args                             % VM arguments
├── rebar.config                            % Build configuration
└── README.md
```

### Supervision Tree

```
happyshopr_sup (one_for_one)
    |
    ├── happyshopr_db (worker) - Manages Mnesia setup/access
    |
    └── happyshopr_http_sup (one_for_one)
            |
            └── cowboy (ranch) - HTTP server
```

---

## 4. Data Model

### Mnesia Tables

#### Table: `shopping_lists`
```erlang
-record(shopping_list, {
    id :: binary(),                    % UUID
    user_id :: binary(),               % User identifier (future use)
    name :: binary(),                  % e.g., "Weekly Shopping"
    created_at :: integer(),           % Unix timestamp
    updated_at :: integer()            % Unix timestamp
}).
```

**Indexes:** `user_id`  
**Type:** `set`  
**Storage:** `disc_copies`

#### Table: `shopping_items`
```erlang
-record(shopping_item, {
    id :: binary(),                    % UUID
    list_id :: binary(),               % Foreign key to shopping_list
    name :: binary(),                  % Item name, e.g., "Milk"
    quantity :: binary() | undefined,  % Optional, e.g., "2 litres"
    notes :: binary() | undefined,     % Optional notes
    category :: binary() | undefined,  % Future: "dairy", "produce", etc.
    recipe_id :: binary() | undefined, % Links to recipe that required this
    recipe_name :: binary() | undefined, % Name of recipe for display
    required :: boolean(),             % Is this item needed for the recipe? (user can toggle)
    completed :: boolean(),            % Mark as acquired (not deleted, remains visible)
    created_at :: integer(),           % Unix timestamp
    added_by :: binary()               % User who added (future use)
}).
```

**Indexes:** `list_id`, `recipe_id`  
**Type:** `set`  
**Storage:** `disc_copies`

#### Table: `api_keys`
```erlang
-record(api_key, {
    key :: binary(),                   % The API key itself (hashed)
    user_id :: binary(),               % Associated user
    description :: binary(),           % e.g., "Claude Integration"
    created_at :: integer(),           % Unix timestamp
    last_used :: integer() | undefined % Last usage timestamp
}).
```

**Type:** `set`  
**Storage:** `disc_copies`

#### Table: `recipes` (Optional - for recipe context)
```erlang
-record(recipe, {
    id :: binary(),                    % UUID
    user_id :: binary(),               % Who saved the recipe
    name :: binary(),                  % Recipe name, e.g., "Spaghetti Carbonara"
    source :: binary() | undefined,    % URL or source of recipe
    created_at :: integer(),           % Unix timestamp
    last_used :: integer() | undefined % When items were last added from this
}).
```

**Type:** `set`  
**Storage:** `disc_copies`

#### Table: `users` (Future - Scaffold Only)
```erlang
-record(user, {
    id :: binary(),                    % UUID
    email :: binary(),                 % Email address
    created_at :: integer()            % Unix timestamp
}).
```

**Type:** `set`  
**Storage:** `disc_copies`

---

## 5. API Specification

### Authentication
All endpoints (except `/health`) require API key authentication.

**Header:** `Authorization: Bearer <api_key>`

**Response Codes:**
- `401 Unauthorized` - Missing or invalid API key
- `403 Forbidden` - Valid key but insufficient permissions

### Base URL
`http://<vm-address>:8080/api/v1`

### Endpoints

#### 1. Health Check
```
GET /health
```

**Response:**
```json
{
  "status": "ok",
  "version": "1.0.0",
  "uptime_seconds": 3600
}
```

**Status Codes:** `200 OK`

---

#### 2. Create Shopping List
```
POST /api/v1/lists
```

**Request Body:**
```json
{
  "name": "Weekly Shopping"
}
```

**Response:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "user_id": "default",
  "name": "Weekly Shopping",
  "created_at": 1699564800,
  "updated_at": 1699564800,
  "items": []
}
```

**Status Codes:**
- `201 Created` - List created successfully
- `400 Bad Request` - Invalid request body
- `401 Unauthorized` - Invalid API key

---

#### 3. Get All Lists
```
GET /api/v1/lists
```

**Response:**
```json
{
  "lists": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "name": "Weekly Shopping",
      "created_at": 1699564800,
      "updated_at": 1699564800,
      "item_count": 5
    }
  ]
}
```

**Status Codes:**
- `200 OK`
- `401 Unauthorized`

---

#### 4. Get Single List
```
GET /api/v1/lists/:list_id
```

**Response:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "user_id": "default",
  "name": "Weekly Shopping",
  "created_at": 1699564800,
  "updated_at": 1699564800,
  "items": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "name": "Milk",
      "quantity": "2 litres",
      "notes": "Semi-skimmed",
      "recipe_id": null,
      "recipe_name": null,
      "required": true,
      "completed": false,
      "created_at": 1699564900
    },
    {
      "id": "660e8400-e29b-41d4-a716-446655440002",
      "name": "Bacon",
      "quantity": "200g",
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "required": true,
      "completed": true,
      "created_at": 1699564900
    }
  ]
}
```

**Notes:**
- Items with `completed: true` remain in the list (not deleted)
- Items show their recipe association via `recipe_id` and `recipe_name`
- `required` indicates if item is needed (can be toggled by user in recipe view)

**Status Codes:**
- `200 OK`
- `404 Not Found` - List doesn't exist
- `401 Unauthorized`

---

#### 5. Update List
```
PUT /api/v1/lists/:list_id
```

**Request Body:**
```json
{
  "name": "Monthly Shopping"
}
```

**Response:**
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "name": "Monthly Shopping",
  "updated_at": 1699564950
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `400 Bad Request`
- `401 Unauthorized`

---

#### 6. Delete List
```
DELETE /api/v1/lists/:list_id
```

**Response:**
```json
{
  "message": "List deleted successfully"
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `401 Unauthorized`

---

#### 7. Add Items to List
```
POST /api/v1/lists/:list_id/items
```

**Request Body:**
```json
{
  "items": [
    {
      "name": "Milk",
      "quantity": "2 litres",
      "notes": "Semi-skimmed"
    },
    {
      "name": "Bread",
      "quantity": "1 loaf"
    }
  ],
  "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
  "recipe_name": "Spaghetti Carbonara"
}
```

**Notes:**
- `recipe_id` and `recipe_name` are optional
- If provided, all items in the batch will be tagged with this recipe
- `recipe_id` can be auto-generated UUID or use existing recipe ID
- All newly created items default to `required: true`

**Response:**
```json
{
  "added": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "name": "Milk",
      "quantity": "2 litres",
      "notes": "Semi-skimmed",
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "required": true,
      "completed": false,
      "created_at": 1699565000
    },
    {
      "id": "660e8400-e29b-41d4-a716-446655440002",
      "name": "Bread",
      "quantity": "1 loaf",
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "required": true,
      "completed": false,
      "created_at": 1699565000
    }
  ]
}
```

**Status Codes:**
- `201 Created`
- `404 Not Found` - List doesn't exist
- `400 Bad Request`
- `401 Unauthorized`

---

#### 8. Get Items by Recipe
```
GET /api/v1/lists/:list_id/recipes/:recipe_id/items
```

**Description:** Get all items associated with a specific recipe

**Response:**
```json
{
  "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
  "recipe_name": "Spaghetti Carbonara",
  "items": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "name": "Spaghetti",
      "quantity": "500g",
      "required": true,
      "completed": false
    },
    {
      "id": "660e8400-e29b-41d4-a716-446655440002",
      "name": "Bacon",
      "quantity": "200g",
      "required": true,
      "completed": true
    },
    {
      "id": "660e8400-e29b-41d4-a716-446655440003",
      "name": "Parsley",
      "quantity": "garnish",
      "required": false,
      "completed": false
    }
  ],
  "total_items": 3,
  "required_items": 2,
  "completed_items": 1
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found` - List or recipe doesn't exist
- `401 Unauthorized`

---

#### 9. Get All Recipes in List
```
GET /api/v1/lists/:list_id/recipes
```

**Description:** Get summary of all recipes that have items in this list

**Response:**
```json
{
  "recipes": [
    {
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "total_items": 5,
      "completed_items": 2,
      "all_completed": false
    },
    {
      "recipe_id": "880e8400-e29b-41d4-a716-446655440004",
      "recipe_name": "Chicken Curry",
      "total_items": 8,
      "completed_items": 8,
      "all_completed": true
    }
  ]
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found` - List doesn't exist
- `401 Unauthorized`

---

#### 10. Update Item
```
PUT /api/v1/lists/:list_id/items/:item_id
```

**Request Body:**
```json
{
  "name": "Whole Milk",
  "quantity": "3 litres",
  "notes": "Organic",
  "completed": false
}
```

**Response:**
```json
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "name": "Whole Milk",
  "quantity": "3 litres",
  "notes": "Organic",
  "completed": false
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `400 Bad Request`
- `401 Unauthorized`

---

#### 11. Delete Item
```
DELETE /api/v1/lists/:list_id/items/:item_id
```

**Response:**
```json
{
  "message": "Item deleted successfully"
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `401 Unauthorized`

---

#### 12. Mark Item as Completed
```
POST /api/v1/lists/:list_id/items/:item_id/complete
```

**Response:**
```json
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "completed": true
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `401 Unauthorized`

---

#### 13. Clear Completed Items
```
DELETE /api/v1/lists/:list_id/items/completed
```

**Description:** Permanently delete items marked as completed. This is optional - completed items can remain in the list indefinitely for reference.

**Response:**
```json
{
  "message": "Cleared 5 completed items",
  "deleted_count": 5
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `401 Unauthorized`

**Notes:**
- This is a destructive operation - items are permanently deleted
- Completed items can remain in the list indefinitely if user prefers
- Useful for "clearing out" old shopping trips

---

#### 14. Toggle Item Required Status
```
POST /api/v1/lists/:list_id/items/:item_id/required
```

**Description:** Toggle whether an item is required for its recipe. Useful when viewing recipe ingredients and deciding which ones you actually need.

**Request Body:**
```json
{
  "required": false
}
```

**Response:**
```json
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "name": "Parsley",
  "recipe_name": "Spaghetti Carbonara",
  "required": false,
  "completed": false
}
```

**Status Codes:**
- `200 OK`
- `404 Not Found`
- `400 Bad Request`
- `401 Unauthorized`

**Use Case:** When viewing a recipe in the web UI, you can mark ingredients as not required (e.g., "I already have parsley" or "I'll skip the optional garnish")

---

## 6. Implementation Details

### Module Responsibilities

#### `happyshopr_app.erl`
- OTP application behaviour
- Start/stop application
- Initialize configuration

#### `happyshopr_sup.erl`
- Main supervisor
- Supervise database and HTTP server supervisors
- `one_for_one` strategy

#### `happyshopr_db.erl`
- Mnesia initialization and schema creation
- Table creation with proper indexes
- CRUD operations wrapper
- Transaction handling
- Database utility functions

**Key Functions:**
```erlang
-export([
    init/0,
    create_tables/0,
    create_list/2,
    get_list/1,
    get_all_lists/1,
    update_list/2,
    delete_list/1,
    add_items/2,
    get_item/1,
    update_item/2,
    delete_item/1,
    mark_completed/1,
    clear_completed/1
]).
```

#### `happyshopr_auth.erl`
- API key validation
- Load API keys from configuration
- Hash and verify keys
- Extract user_id from API key

**Key Functions:**
```erlang
-export([
    init/0,
    validate_key/1,
    get_user_id/1,
    hash_key/1
]).
```

#### `happyshopr_lists.erl`
- Business logic layer
- Validation rules
- Data transformation
- UUID generation

#### Handler Modules
- Extract request data
- Validate input
- Call business logic
- Format responses
- Handle errors consistently

### Cowboy Routing

```erlang
Dispatch = cowboy_router:compile([
    {'_', [
        % Static files
        {"/", cowboy_static, {priv_file, happyshopr, "static/index.html"}},
        {"/[...]", cowboy_static, {priv_dir, happyshopr, "static"}},
        
        % API endpoints
        {"/health", health_handler, []},
        {"/api/v1/lists", lists_handler, []},
        {"/api/v1/lists/:list_id", lists_handler, []},
        {"/api/v1/lists/:list_id/items", items_handler, []},
        {"/api/v1/lists/:list_id/items/:item_id", items_handler, []},
        {"/api/v1/lists/:list_id/items/:item_id/complete", items_handler, [complete]},
        {"/api/v1/lists/:list_id/items/:item_id/required", items_handler, [required]},
        {"/api/v1/lists/:list_id/items/completed", items_handler, [clear_completed]},
        {"/api/v1/lists/:list_id/recipes", items_handler, [recipes_summary]},
        {"/api/v1/lists/:list_id/recipes/:recipe_id/items", items_handler, [recipe_items]}
    ]}
]),
```

### Error Handling

All endpoints should return consistent error responses:

```json
{
  "error": {
    "code": "not_found",
    "message": "List not found",
    "details": {}
  }
}
```

**Error Codes:**
- `bad_request` - Invalid input
- `unauthorized` - Missing/invalid API key
- `forbidden` - Valid key but no permission
- `not_found` - Resource doesn't exist
- `internal_error` - Server error

### Logging

Use standard OTP logging (logger module):
- Info: Successful operations, startup
- Warning: Invalid requests, auth failures
- Error: Database errors, crashes

---

## 7. Web Frontend Specification

### Overview
A single-page application (SPA) that provides a clean, mobile-friendly interface for managing shopping lists. The frontend is served as static files from Cowboy and communicates with the backend via REST API.

### User Interface Components

#### 1. Main Layout
```
┌─────────────────────────────────────┐
│  HappyShopr 🛒                   ⚙️ │
├─────────────────────────────────────┤
│                                     │
│  [All Items] [By Recipe]            │
│  [ ] Hide Completed                 │
│                                     │
│  ┌───────────────────────────────┐ │
│  │ Spaghetti Carbonara      [5/8]│ │
│  │ ○ Spaghetti 500g              │ │
│  │ ✓ Bacon 200g  (strikethrough) │ │
│  │ ○ Eggs (4)                    │ │
│  │ ✓ Parmesan  (strikethrough)   │ │
│  │ ○ Black pepper                │ │
│  │ [✓] Parsley - NOT NEEDED      │ │
│  └───────────────────────────────┘ │
│                                     │
│  ┌───────────────────────────────┐ │
│  │ General Items                 │ │
│  │ ○ Milk 2 litres               │ │
│  │ ✓ Bread  (strikethrough)      │ │
│  └───────────────────────────────┘ │
│                                     │
│  [+ Add Items]                      │
│                                     │
│  [Clear Completed (3)]              │
└─────────────────────────────────────┘
```

**Visual States:**
- ○ = Not acquired, required
- ✓ = Acquired (strikethrough text, remains visible)
- [✓] = Acquired but marked as "not required"
- Strikethrough text for completed items
- Dimmed styling for "not required" items

#### 2. Add Items Modal
```
┌─────────────────────────────────────┐
│  Add Items                       [×]│
├─────────────────────────────────────┤
│                                     │
│  Recipe (optional):                 │
│  [Chicken Curry____________]        │
│                                     │
│  Items (one per line):              │
│  ┌───────────────────────────────┐ │
│  │ Milk 2 litres                 │ │
│  │ Bread                         │ │
│  │ Eggs 6                        │ │
│  │                               │ │
│  └───────────────────────────────┘ │
│                                     │
│  [Cancel]              [Add Items] │
└─────────────────────────────────────┘
```

#### 3. Recipe Detail View
```
┌─────────────────────────────────────┐
│  ← Back   Spaghetti Carbonara  [5/8]│
├─────────────────────────────────────┤
│                                     │
│  Required Ingredients:              │
│  ○ Spaghetti 500g         [Not Req]│
│  ✓ Bacon 200g             [Not Req]│
│  ○ Eggs (4)               [Not Req]│
│  ✓ Parmesan 100g          [Not Req]│
│  ○ Black pepper           [Not Req]│
│                                     │
│  Not Required:                      │
│  ✓ Parsley (garnish)      [Required]│
│                                     │
│  [+ Add More Items to Recipe]       │
└─────────────────────────────────────┘
```

**Features:**
- View all ingredients for a specific recipe
- Toggle "required" status with button next to each item
- Items marked "not required" move to separate section
- Can still mark not-required items as completed
- Add more items to this recipe
- Back button returns to main list view

#### 4. Recipe Filter View
```
┌─────────────────────────────────────┐
│  Recipes                            │
├─────────────────────────────────────┤
│                                     │
│  ✓ Chicken Curry              [8/8]│
│  ○ Spaghetti Carbonara        [3/5]│
│  ○ Shepherd's Pie             [0/6]│
│                                     │
│  [Click to view recipe items]       │
└─────────────────────────────────────┘
```

### Features

#### Core Features (MVP)
1. **List View**
   - Display all shopping items
   - Toggle completed/uncompleted
   - Swipe to delete (mobile)
   - Click to edit item details

2. **Recipe Grouping**
   - Group items by recipe
   - Show completion progress per recipe
   - Collapse/expand recipe groups
   - Filter to show only items from specific recipe

3. **Add Items**
   - Quick add via modal
   - Optional recipe assignment
   - Parse multiple items from text (one per line)
   - Auto-detect quantity from text (e.g., "Milk 2 litres")

4. **Manage Items**
   - Mark as completed/uncompleted (items remain visible when completed)
   - Toggle "required" status (useful in recipe view)
   - Edit item details
   - Delete items permanently
   - Clear all completed items (optional, permanent deletion)

5. **Settings**
   - Configure API endpoint and key
   - Select default list
   - View connection status

#### Nice-to-Have Features (v2)
- Offline support with localStorage sync
- Drag-and-drop reordering
- Share list via URL
- Print shopping list
- Dark mode
- Voice input for adding items
- Barcode scanner integration

### File Structure

#### index.html
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>HappyShopr</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <link rel="stylesheet" href="/css/custom.css">
</head>
<body class="bg-gray-50">
    <div id="app">
        <!-- App will be rendered here -->
    </div>
    
    <!-- Modals -->
    <div id="add-items-modal" class="hidden">...</div>
    <div id="settings-modal" class="hidden">...</div>
    
    <script type="module" src="/js/app.js"></script>
</body>
</html>
```

#### js/api.js
API client module for backend communication:
```javascript
// API client wrapper
class HappyShoprAPI {
    constructor(baseUrl, apiKey) {
        this.baseUrl = baseUrl;
        this.apiKey = apiKey;
    }
    
    async getLists() { ... }
    async getList(listId) { ... }
    async addItems(listId, items, recipeId, recipeName) { ... }
    async updateItem(listId, itemId, updates) { ... }
    async deleteItem(listId, itemId) { ... }
    async markCompleted(listId, itemId) { ... }
    async clearCompleted(listId) { ... }
    async getRecipes(listId) { ... }
    async getRecipeItems(listId, recipeId) { ... }
}
```

#### js/app.js
Main application logic:
```javascript
// Application state management
class AppState {
    constructor() {
        this.currentList = null;
        this.items = [];
        this.recipes = [];
        this.viewMode = 'all'; // 'all' or 'recipe'
        this.selectedRecipe = null;
    }
    
    async init() { ... }
    async loadList() { ... }
    async loadRecipes() { ... }
    filterItemsByRecipe(recipeId) { ... }
}

// Event handlers
function handleItemToggle(itemId) { ... }
function handleAddItems(text, recipeName) { ... }
function handleDeleteItem(itemId) { ... }
```

#### js/components/list-view.js
List rendering component:
```javascript
function renderListView(items, groupByRecipe) {
    if (groupByRecipe) {
        return renderRecipeGroupedView(items);
    } else {
        return renderFlatListView(items);
    }
}

function renderRecipeGroupedView(items) {
    // Group items by recipe_id
    // Render collapsible sections per recipe
}

function renderFlatListView(items) {
    // Render all items in a single list
}
```

#### js/components/item-form.js
Add items form component:
```javascript
function showAddItemsModal() { ... }
function parseItemsText(text) {
    // Parse lines like:
    // "Milk 2 litres"
    // "Bread"
    // "Eggs 6"
    // Returns array of {name, quantity}
}
```

#### js/components/recipe-filter.js
Recipe filtering component:
```javascript
function renderRecipesSummary(recipes) {
    // Show list of recipes with completion status
    // Allow clicking to filter items
}
```

### Styling Guidelines

#### Color Scheme
- Primary: Blue (#3B82F6)
- Success: Green (#10B981)
- Danger: Red (#EF4444)
- Background: Gray-50 (#F9FAFB)
- Text: Gray-900 (#111827)

#### Mobile-First Design
- Touch-friendly buttons (min 44px tap target)
- Swipe gestures for common actions
- Fixed header with list name
- Sticky "Add Items" button
- Pull-to-refresh

#### Responsive Breakpoints
- Mobile: < 640px (single column)
- Tablet: 640px - 1024px (single column, larger text)
- Desktop: > 1024px (max-width 800px centered)

### Local Storage

Store settings and cache data:
```javascript
localStorage.setItem('happyshopr_config', JSON.stringify({
    apiUrl: 'http://...',
    apiKey: '...',
    defaultListId: '...'
}));

// Cache for offline support
localStorage.setItem('happyshopr_cache', JSON.stringify({
    lastSync: Date.now(),
    items: [...],
    recipes: [...]
}));
```

### User Workflows

#### Workflow 1: First-Time Setup
1. User visits http://your-vm:8080
2. App checks localStorage for config
3. If not found, show settings modal
4. User enters API key (or use default)
5. App loads default list
6. User can start adding items

#### Workflow 2: Adding Items from Recipe
1. User clicks "Add Items"
2. Enters recipe name: "Chicken Curry"
3. Enters items (one per line):
   ```
   Chicken breast 500g
   Coconut milk
   Curry paste
   Rice 2 cups
   ```
4. Clicks "Add Items"
5. App parses items and sends to API with recipe metadata
6. Items appear grouped under "Chicken Curry"

#### Workflow 3: Shopping at Store
1. User opens app on phone
2. Views list grouped by recipe
3. Taps items to mark as completed (they remain visible with strikethrough)
4. Visual progress shown per recipe
5. Can filter to show only incomplete items (checkbox: "Hide Completed")
6. After shopping, optionally clicks "Clear Completed" to permanently delete completed items

#### Workflow 4: Viewing Recipe Progress
1. User switches to "By Recipe" view
2. Sees list of recipes with completion ratios
3. Clicks "Chicken Curry (5/8)"
4. Sees only items needed for that recipe
5. Can quickly see what's still needed

#### Workflow 5: Managing Recipe Ingredients
1. User views recipe in detail (clicks on recipe name)
2. Sees all ingredients for "Spaghetti Carbonara"
3. Notices optional ingredient: "Parsley (garnish)"
4. Clicks "[Not Required]" button next to Parsley
5. Parsley moves to "Not Required" section and is dimmed
6. Progress counter updates: "7/7" (now only counting required items)
7. Can later mark not-required items as required again if needed

### Error Handling

#### Connection Errors
```javascript
if (!navigator.onLine) {
    showMessage('You are offline. Changes will sync when back online.', 'warning');
    // Use cached data
}

try {
    await api.addItems(...);
} catch (error) {
    if (error.status === 401) {
        showMessage('Authentication failed. Check your API key.', 'error');
    } else {
        showMessage('Failed to add items. Try again.', 'error');
    }
}
```

#### API Errors
- 401: Show settings, prompt for new API key
- 404: List not found, offer to create or select different list
- 500: Server error, show retry button
- Network timeout: Use cached data, queue changes for sync

### Accessibility

- Semantic HTML (lists, buttons, forms)
- ARIA labels for icons and actions
- Keyboard navigation support
- Focus indicators
- Screen reader announcements for state changes
- Color contrast ratios meet WCAG AA

### Performance

- Lazy load components
- Debounce API calls (save draft)
- Optimistic UI updates
- Request batching for multiple operations
- Image optimization (if recipe images added later)
- Service Worker for offline capability (v2)

---

## 8. Configuration

### sys.config
```erlang
[
    {happyshopr, [
        {http_port, 8080},
        {api_keys, [
            {<<"your-secure-api-key-here">>, <<"default">>}
        ]},
        {mnesia_dir, "/var/lib/happyshopr/mnesia"}
    ]},
    {mnesia, [
        {dir, "/var/lib/happyshopr/mnesia"}
    ]}
].
```

### vm.args
```
-name happyshopr@127.0.0.1
-setcookie happyshopr_secret_cookie
+K true
+A 32
-env ERL_MAX_PORTS 4096
```

---

## 9. Deployment Strategy

### Release Build
```bash
rebar3 as prod release
```

### Directory Structure on VM
```
/opt/happyshopr/
├── bin/
│   ├── happyshopr           # Start script
│   └── happyshopr-admin     # Admin utilities
├── lib/                     # Compiled beam files
├── releases/                # Release versions
└── /var/lib/happyshopr/
    └── mnesia/              # Mnesia data files
```

### systemd Service

Create `/etc/systemd/system/happyshopr.service`:

```ini
[Unit]
Description=HappyShopr Shopping List Service
After=network.target

[Service]
Type=forking
User=happyshopr
Group=happyshopr
WorkingDirectory=/opt/happyshopr
ExecStart=/opt/happyshopr/bin/happyshopr start
ExecStop=/opt/happyshopr/bin/happyshopr stop
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

### Initial Setup Commands
```bash
# Create user
sudo useradd -r -s /bin/false happyshopr

# Create directories
sudo mkdir -p /opt/happyshopr
sudo mkdir -p /var/lib/happyshopr/mnesia
sudo chown -R happyshopr:happyshopr /opt/happyshopr
sudo chown -R happyshopr:happyshopr /var/lib/happyshopr

# Deploy release
sudo cp -r _build/prod/rel/happyshopr/* /opt/happyshopr/

# Enable and start service
sudo systemctl daemon-reload
sudo systemctl enable happyshopr
sudo systemctl start happyshopr
```

---

## 10. Testing Strategy

### Unit Tests
- Test each module in isolation
- Mock Mnesia operations
- Use EUnit

### Integration Tests
- Test full API endpoints
- Use Common Test
- Test with actual Mnesia database (test schema)

### Manual Testing
```bash
# Health check
curl http://localhost:8080/health

# Create list
curl -X POST http://localhost:8080/api/v1/lists \
  -H "Authorization: Bearer your-api-key" \
  -H "Content-Type: application/json" \
  -d '{"name": "Test List"}'

# Add items
curl -X POST http://localhost:8080/api/v1/lists/{list_id}/items \
  -H "Authorization: Bearer your-api-key" \
  -H "Content-Type: application/json" \
  -d '{"items": [{"name": "Milk"}, {"name": "Bread"}]}'
```

---

## 11. Future Enhancements (Architecture Prep)

### Multi-User Support
The data model already includes `user_id` fields. To implement:

1. Add user registration endpoint
2. Implement email-based magic link authentication
3. Store sessions/tokens in Mnesia
4. Filter all queries by user_id

### Email Magic Links
```erlang
-record(auth_token, {
    token :: binary(),           % One-time token
    email :: binary(),           % User email
    expires_at :: integer(),     % Expiry timestamp
    used :: boolean()            % Prevent reuse
}).
```

**Flow:**
1. User requests login with email
2. Generate unique token, send email
3. User clicks link with token
4. Validate token, create API key
5. Return API key for use

### Categories & Smart Features
- Auto-categorize items
- Suggest quantities based on history
- Recipe parsing integration

---

## 12. Development Workflow

### Phase 1: Core Setup (Day 1)
1. Initialize rebar3 project
2. Set up supervision tree
3. Initialize Mnesia with tables
4. Basic Cowboy HTTP server
5. Health endpoint

### Phase 2: Authentication (Day 1-2)
1. Implement API key loading
2. Authentication middleware
3. Test auth success/failure

### Phase 3: Lists CRUD (Day 2-3)
1. Create list endpoint
2. Get lists endpoint
3. Update/delete list endpoints
4. Test with curl

### Phase 4: Items CRUD (Day 3-4)
1. Add items endpoint
2. Update/delete item endpoints
3. Mark completed endpoint
4. Clear completed endpoint

### Phase 5: Release & Deploy (Day 4-5)
1. Configure relx
2. Build release
3. Test on VM
4. Create systemd service
5. Document deployment

### Phase 6: Web Frontend (Day 5-7)
1. Create static HTML structure
2. Implement API client (js/api.js)
3. Build core UI components
4. Add recipe grouping functionality
5. Implement add items modal
6. Test on mobile devices
7. Polish responsive design

---

## 13. Claude Integration

### Project Knowledge Content
```
HappyShopr API Configuration
===============================

Base URL: http://your-vm-address:8080/api/v1
API Key: your-secure-api-key-here

Authentication:
Use Bearer token authentication with all requests except /health.
Header: Authorization: Bearer your-secure-api-key-here

Primary List ID: 550e8400-e29b-41d4-a716-446655440000
(Create this on first run)

Available Endpoints:
- POST /lists - Create new list
- GET /lists - Get all lists
- GET /lists/:id - Get specific list with items
- POST /lists/:id/items - Add items (accepts array + optional recipe info)
- DELETE /lists/:id/items/:item_id - Remove item
- POST /lists/:id/items/:item_id/complete - Mark completed
- DELETE /lists/:id/items/completed - Clear completed items
- GET /lists/:id/recipes - Get all recipes in list
- GET /lists/:id/recipes/:recipe_id/items - Get items for specific recipe

Recipe Tagging:
When adding items from a recipe, include recipe_id and recipe_name in the request:
{
  "items": [...],
  "recipe_id": "generated-uuid",
  "recipe_name": "Spaghetti Carbonara"
}

Context Awareness:
- If user is discussing a recipe, automatically generate a recipe_id and use the recipe name
- If user says "add these ingredients", use the recipe context from the conversation
- Keep track of recipe_id during the conversation for follow-up additions

Example: User discussing Chicken Curry recipe
User: "I want to make Chicken Curry tonight"
Claude: [Notes recipe name: "Chicken Curry", generates UUID]

User: "Add the ingredients to my list"
Claude: [Calls API with recipe_id and recipe_name="Chicken Curry"]

When user says "add X to shopping list", use the primary list ID above.
```

### Conversation Examples

**Example 1: Recipe Context**
```
User: I'm looking at this recipe for Spaghetti Carbonara. The ingredients are:
- 400g spaghetti
- 200g bacon
- 4 eggs
- 100g parmesan
- Black pepper

Claude: Great recipe! Would you like me to add these ingredients to your HappyShopr list?

User: Yes please

Claude: [Generates UUID for recipe, calls POST /lists/{list_id}/items with recipe_name="Spaghetti Carbonara"]
I've added 5 items to your shopping list under "Spaghetti Carbonara". You can view them grouped by recipe in the web interface.
```

**Example 2: Adding to Existing Recipe**
```
User: For that Chicken Curry recipe, I also need garlic and ginger

Claude: [Uses previously stored recipe_id for Chicken Curry]
Added garlic and ginger to your Chicken Curry recipe items.
```

**Example 3: General Items**
```
User: Add milk and bread to my list

Claude: [Calls API without recipe_id - these are general items]
Added milk and bread to your shopping list.
```

---

## 14. Risks & Mitigation

### Risk: Mnesia Data Loss
**Mitigation:** 
- Use `disc_copies` storage
- Regular backups via mnesia:backup/1
- Document restore procedure

### Risk: API Key Exposure
**Mitigation:**
- Never log API keys
- Store hashed in Mnesia
- Rotate keys periodically

### Risk: Service Downtime
**Mitigation:**
- systemd auto-restart
- Health monitoring
- OTP supervision handles crashes

### Risk: Concurrent Access Issues
**Mitigation:**
- Mnesia transactions for writes
- Proper record locking
- Test concurrent operations

---

## 15. Acceptance Criteria

The implementation is complete when:

- [ ] All API endpoints respond correctly
- [ ] API key authentication works
- [ ] Mnesia persists data across restarts
- [ ] Release builds successfully
- [ ] Service runs on VM via systemd
- [ ] Claude can add/remove/view items via API
- [ ] Claude can tag items with recipe information
- [ ] Health endpoint returns service status
- [ ] Error responses are consistent
- [ ] Basic logging is in place
- [ ] Web frontend loads and displays lists
- [ ] Recipe grouping works in UI
- [ ] Add items modal accepts recipe name
- [ ] Items can be marked completed in UI
- [ ] UI is mobile-responsive
- [ ] README documents deployment

---

## 16. Quick Reference

### Generate API Key
```bash
# Generate a secure random key
openssl rand -base64 32
```

### Check Service Status
```bash
sudo systemctl status happyshopr
```

### View Logs
```bash
sudo journalctl -u happyshopr -f
```

### Hot Upgrade (Future)
```bash
/opt/happyshopr/bin/happyshopr upgrade 1.1.0
```

---

## Appendix A: rebar.config Template

```erlang
{erl_opts, [debug_info]}.

{deps, [
    {cowboy, "2.12.0"},
    {jsx, "3.1.0"}
]}.

{relx, [
    {release, {happyshopr, "1.0.0"}, [
        happyshopr,
        sasl
    ]},
    {mode, prod},
    {include_erts, true},
    {extended_start_script, true},
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"}
]}.

{profiles, [
    {prod, [
        {erl_opts, [no_debug_info, warnings_as_errors]}
    ]}
]}.

{plugins, []}.
```

---

## Appendix B: Example Mnesia Initialization

```erlang
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_tables().

create_tables() ->
    mnesia:create_table(shopping_list, [
        {attributes, record_info(fields, shopping_list)},
        {disc_copies, [node()]},
        {index, [user_id]},
        {type, set}
    ]),
    mnesia:create_table(shopping_item, [
        {attributes, record_info(fields, shopping_item)},
        {disc_copies, [node()]},
        {index, [list_id]},
        {type, set}
    ]),
    mnesia:create_table(api_key, [
        {attributes, record_info(fields, api_key)},
        {disc_copies, [node()]},
        {type, set}
    ]),
    mnesia:wait_for_tables([shopping_list, shopping_item, api_key], 5000).
```

---

**End of Planning Document**

This document provides Claude Code with everything needed to implement HappyShopr. Update as implementation progresses and requirements evolve.
