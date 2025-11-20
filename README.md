# HappyShopr 🛒

A persistent shopping list service built with Erlang/OTP, designed for integration with Claude AI. Manage shopping lists via REST API or web interface with support for recipe grouping and intelligent item tracking. fr

## Features

- ✅ **REST API** with Bearer token authentication
- ✅ **Persistent storage** using Mnesia (survives restarts)
- ✅ **Recipe grouping** - organize items by recipe
- ✅ **Smart completion** - completed items remain visible (not deleted)
- ✅ **Required/optional** items - mark ingredients as needed/not needed
- ✅ **Web interface** - mobile-friendly SPA
- ✅ **Claude integration** - API designed for AI assistant use

## Architecture

### Backend
- **Language:** Erlang/OTP 25+
- **HTTP Server:** Cowboy 2.x
- **Database:** Mnesia (distributed Erlang database)
- **JSON:** jsx
- **Build:** rebar3

### Frontend
- **Framework:** Vanilla JavaScript (ES6 modules)
- **Styling:** Tailwind CSS (via CDN)
- **API Client:** Fetch API
- **Storage:** localStorage for configuration

## Quick Start

### Prerequisites

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install erlang rebar3

# macOS
brew install erlang rebar3

# Fedora/RHEL
sudo dnf install erlang rebar3
```

### Build and Run

```bash
# Clone the repository
git clone <repository-url>
cd HappyShopr

# Fetch dependencies and compile
rebar3 compile

# Run in development mode (requires node name for Mnesia disc_copies)
rebar3 shell --name happyshopr@172.233.189.66

# Or build a release
rebar3 release
_build/default/rel/happyshopr/bin/happyshopr console
```

The application will start on `http://172.233.189.66:8080`

### First-Time Setup

1. Open `http://172.233.189.66:8080` in your browser
2. Click the Settings button (⚙️)
3. Configure:
   - **API URL:** `http://172.233.189.66:8080`
   - **API Key:** `demo-api-key-replace-in-production` (default)
   - **List ID:** Leave empty to create a new list
4. Click Save

The app will automatically create a default shopping list on first load.

## API Reference

### Authentication

All endpoints except `/health` require Bearer token authentication:

```bash
Authorization: Bearer <your-api-key>
```

### Base URL

```
http://172.233.189.66:8080/api/v1
```

### Endpoints

#### Health Check
```bash
GET /health

# Response
{
  "status": "ok",
  "version": "1.0.0",
  "uptime_seconds": 3600
}
```

#### Create Shopping List
```bash
POST /api/v1/lists
Authorization: Bearer <api-key>
Content-Type: application/json

{
  "name": "Weekly Shopping"
}

# Response (201 Created)
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "user_id": "default",
  "name": "Weekly Shopping",
  "created_at": 1699564800,
  "updated_at": 1699564800,
  "items": []
}
```

#### Get All Lists
```bash
GET /api/v1/lists
Authorization: Bearer <api-key>

# Response
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

#### Get Single List with Items
```bash
GET /api/v1/lists/:list_id
Authorization: Bearer <api-key>

# Response
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "user_id": "default",
  "name": "Weekly Shopping",
  "items": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "name": "Milk",
      "quantity": "2 litres",
      "required": true,
      "completed": false,
      "created_at": 1699564900
    }
  ]
}
```

#### Add Items to List
```bash
POST /api/v1/lists/:list_id/items
Authorization: Bearer <api-key>
Content-Type: application/json

{
  "items": [
    {
      "name": "Spaghetti",
      "quantity": "500g"
    },
    {
      "name": "Bacon",
      "quantity": "200g"
    }
  ],
  "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
  "recipe_name": "Spaghetti Carbonara"
}

# Response (201 Created)
{
  "added": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "name": "Spaghetti",
      "quantity": "500g",
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "required": true,
      "completed": false,
      "created_at": 1699565000
    }
  ]
}
```

#### Mark Item as Completed
```bash
POST /api/v1/lists/:list_id/items/:item_id/complete
Authorization: Bearer <api-key>
Content-Type: application/json

{
  "completed": true
}

# Response
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "completed": true
}
```

**Note:** Completed items remain in the database with strikethrough styling in the UI. They are NOT deleted.

#### Toggle Required Status
```bash
POST /api/v1/lists/:list_id/items/:item_id/required
Authorization: Bearer <api-key>
Content-Type: application/json

{
  "required": false
}

# Response
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "name": "Parsley",
  "recipe_name": "Spaghetti Carbonara",
  "required": false,
  "completed": false
}
```

#### Clear Completed Items (Permanent Deletion)
```bash
DELETE /api/v1/lists/:list_id/items/completed
Authorization: Bearer <api-key>

# Response
{
  "message": "Cleared 5 completed items",
  "deleted_count": 5
}
```

#### Get Recipes Summary
```bash
GET /api/v1/lists/:list_id/recipes
Authorization: Bearer <api-key>

# Response
{
  "recipes": [
    {
      "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
      "recipe_name": "Spaghetti Carbonara",
      "total_items": 5,
      "completed_items": 2,
      "all_completed": false
    }
  ]
}
```

#### Update Item
```bash
PUT /api/v1/lists/:list_id/items/:item_id
Authorization: Bearer <api-key>
Content-Type: application/json

{
  "name": "Whole Milk",
  "quantity": "3 litres",
  "notes": "Organic"
}
```

#### Delete Item
```bash
DELETE /api/v1/lists/:list_id/items/:item_id
Authorization: Bearer <api-key>

# Response
{
  "message": "Item deleted successfully"
}
```

## Configuration

### API Keys

Edit `config/sys.config` to add or change API keys:

```erlang
{happyshopr, [
    {http_port, 8080},
    {api_keys, [
        {<<"your-secure-api-key-here">>, <<"default">>},
        {<<"another-key">>, <<"user2">>}
    ]},
    {mnesia_dir, "./data/mnesia"}
]},
```

**Generate a secure API key:**
```bash
openssl rand -base64 32
```

### HTTP Port

Change the port in `config/sys.config`:

```erlang
{http_port, 8080}
```

## Production Deployment

### Build Production Release

```bash
rebar3 as prod release
```

This creates a self-contained release in `_build/prod/rel/happyshopr/`

### Deploy to Server

1. **Copy release to server:**
```bash
scp -r _build/prod/rel/happyshopr user@server:/opt/
```

2. **Create systemd service** (`/etc/systemd/system/happyshopr.service`):
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

3. **Create user and set permissions:**
```bash
sudo useradd -r -s /bin/false happyshopr
sudo mkdir -p /var/lib/happyshopr/mnesia
sudo chown -R happyshopr:happyshopr /opt/happyshopr
sudo chown -R happyshopr:happyshopr /var/lib/happyshopr
```

4. **Start service:**
```bash
sudo systemctl daemon-reload
sudo systemctl enable happyshopr
sudo systemctl start happyshopr
sudo systemctl status happyshopr
```

### View Logs

```bash
# Systemd logs
sudo journalctl -u happyshopr -f

# Application logs (if running in console)
/opt/happyshopr/bin/happyshopr console
```

## Claude AI Integration

### Project Knowledge Configuration

Add this to your Claude project knowledge:

```
HappyShopr API Configuration
==============================

Base URL: http://your-server:8080/api/v1
API Key: your-secure-api-key-here
Primary List ID: <your-list-id>

When the user mentions recipes or ingredients:
1. Generate a UUID for the recipe
2. Include recipe_id and recipe_name in the API call
3. Track recipe_id for follow-up additions

Example: User says "Add ingredients for Chicken Curry"
- Generate recipe_id: 770e8400-e29b-41d4-a716-446655440003
- Use recipe_name: "Chicken Curry"
- Add all items with this recipe context
```

### Usage Examples

**User:** "I'm making Spaghetti Carbonara tonight. Here are the ingredients..."

**Claude:**
```python
# Generates UUID for recipe
recipe_id = "770e8400-e29b-41d4-a716-446655440003"

# Calls API
POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Spaghetti", "quantity": "500g"},
    {"name": "Bacon", "quantity": "200g"},
    {"name": "Eggs", "quantity": "4"},
    {"name": "Parmesan", "quantity": "100g"}
  ],
  "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
  "recipe_name": "Spaghetti Carbonara"
}
```

**User:** "Also add garlic to that recipe"

**Claude:**
```python
# Uses same recipe_id from context
POST /api/v1/lists/{list_id}/items
{
  "items": [
    {"name": "Garlic", "quantity": "3 cloves"}
  ],
  "recipe_id": "770e8400-e29b-41d4-a716-446655440003",
  "recipe_name": "Spaghetti Carbonara"
}
```

## Key Behavioral Features

### Completed Items Remain Visible

**Important:** When you mark an item as "completed", it is NOT deleted. It remains in the database and is shown with a strikethrough in the UI. This allows you to:

- See what you've already bought during shopping
- Review purchase history
- Undo completion if needed
- Optionally clear completed items later

To permanently delete completed items, use the "Clear Completed" button or endpoint.

### Required vs. Optional Items

Each item has a `required` field (defaults to `true`). This is useful for recipe management:

- **Required:** Item is needed for the recipe (counts toward progress)
- **Not Required:** Already have it, or skipping this ingredient

Progress counters only count required items. Example:
- Recipe has 5 items, you mark 2 as "not required"
- Progress shows: 0/3 instead of 0/5

## Troubleshooting

### Mnesia "bad_type" or "disc_copies" Errors

If you see errors like `{bad_type, table_name, disc_copies, nonode@nohost}`, you need to start the shell with a node name:

```bash
# Instead of: rebar3 shell
# Use:
rebar3 shell --name happyshopr@172.233.189.66
```

Mnesia's `disc_copies` storage requires a distributed Erlang node with a proper name.

### Port Already in Use

Change the port in `config/sys.config` or kill the process:
```bash
lsof -ti:8080 | xargs kill -9
```

### Mnesia Database Issues

Reset the database (WARNING: deletes all data):
```bash
rm -rf data/mnesia/*
# Restart the application to recreate schema
```

### API Key Not Working

1. Check `config/sys.config` has the correct key
2. Ensure you're using Bearer token format
3. Restart the application after changing config

### CORS Issues

If accessing from a different domain, you may need to add CORS headers to the Cowboy handlers.

## Development

### Project Structure

```
happyshopr/
├── apps/happyshopr/
│   ├── src/
│   │   ├── happyshopr_app.erl      # Application entry point
│   │   ├── happyshopr_sup.erl      # Main supervisor
│   │   ├── happyshopr_http_sup.erl # HTTP server supervisor
│   │   ├── happyshopr_db.erl       # Mnesia database interface
│   │   ├── happyshopr_auth.erl     # Authentication
│   │   ├── happyshopr_handler.erl  # Common handler utilities
│   │   └── handlers/
│   │       ├── health_handler.erl  # Health check
│   │       ├── lists_handler.erl   # List CRUD
│   │       └── items_handler.erl   # Item CRUD + recipes
│   ├── include/
│   │   └── happyshopr.hrl          # Record definitions
│   └── priv/
│       └── static/                 # Web frontend
│           ├── index.html
│           ├── css/
│           └── js/
│               ├── api.js          # API client
│               └── app.js          # Main app
├── config/
│   ├── sys.config                  # Runtime configuration
│   └── vm.args                     # VM arguments
└── rebar.config                    # Build configuration
```

### Running Tests

```bash
# Unit tests (when implemented)
rebar3 eunit

# Integration tests (when implemented)
rebar3 ct
```

### Manual API Testing

```bash
# Health check
curl http://172.233.189.66:8080/health

# Create list
curl -X POST http://172.233.189.66:8080/api/v1/lists \
  -H "Authorization: Bearer demo-api-key-replace-in-production" \
  -H "Content-Type: application/json" \
  -d '{"name": "Test List"}'

# Add items with recipe
curl -X POST http://172.233.189.66:8080/api/v1/lists/<list-id>/items \
  -H "Authorization: Bearer demo-api-key-replace-in-production" \
  -H "Content-Type: application/json" \
  -d '{
    "items": [
      {"name": "Milk", "quantity": "2 litres"},
      {"name": "Bread"}
    ],
    "recipe_id": "test-recipe-123",
    "recipe_name": "Breakfast"
  }'

# Get list with items
curl http://172.233.189.66:8080/api/v1/lists/<list-id> \
  -H "Authorization: Bearer demo-api-key-replace-in-production"
```

## License

Apache 2.0

## Contributing

Contributions welcome! Please open an issue or submit a pull request.

## Support

For issues and questions, please open a GitHub issue.
