#!/bin/bash
# HappyShopr Development Helper Script

set -e

CMD=$1

case "$CMD" in
    "compile")
        echo "🔨 Compiling HappyShopr..."
        rebar3 compile
        echo "✅ Compilation complete!"
        ;;

    "run")
        echo "🚀 Starting HappyShopr in development mode..."
        rebar3 shell
        ;;

    "release")
        echo "📦 Building production release..."
        rebar3 as prod release
        echo "✅ Release built at _build/prod/rel/happyshopr/"
        ;;

    "clean")
        echo "🧹 Cleaning build artifacts..."
        rebar3 clean
        rm -rf _build/
        rm -rf data/
        echo "✅ Clean complete!"
        ;;

    "test")
        echo "🧪 Running tests..."
        rebar3 eunit
        rebar3 ct
        echo "✅ Tests complete!"
        ;;

    "health")
        echo "🏥 Checking health endpoint..."
        curl -s http://localhost:8080/health | json_pp || echo "❌ Service not running"
        ;;

    "api-test")
        API_KEY=${2:-"demo-api-key-replace-in-production"}
        echo "🔧 Testing API with key: $API_KEY"

        echo -e "\n1. Health check:"
        curl -s http://localhost:8080/health | json_pp

        echo -e "\n2. Creating test list:"
        LIST_RESPONSE=$(curl -s -X POST http://localhost:8080/api/v1/lists \
            -H "Authorization: Bearer $API_KEY" \
            -H "Content-Type: application/json" \
            -d '{"name": "Test Shopping List"}')
        echo "$LIST_RESPONSE" | json_pp

        LIST_ID=$(echo "$LIST_RESPONSE" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)

        if [ -n "$LIST_ID" ]; then
            echo -e "\n3. Adding items to list $LIST_ID:"
            curl -s -X POST "http://localhost:8080/api/v1/lists/$LIST_ID/items" \
                -H "Authorization: Bearer $API_KEY" \
                -H "Content-Type: application/json" \
                -d '{
                    "items": [
                        {"name": "Milk", "quantity": "2 litres"},
                        {"name": "Bread", "quantity": "1 loaf"}
                    ],
                    "recipe_id": "test-recipe-123",
                    "recipe_name": "Breakfast Items"
                }' | json_pp

            echo -e "\n4. Getting list with items:"
            curl -s "http://localhost:8080/api/v1/lists/$LIST_ID" \
                -H "Authorization: Bearer $API_KEY" | json_pp
        fi
        ;;

    "logs")
        echo "📋 Showing recent logs..."
        if [ -f "erl_crash.dump" ]; then
            echo "⚠️  Crash dump found:"
            tail -n 50 erl_crash.dump
        fi
        ;;

    "format")
        echo "🎨 Formatting Erlang code..."
        find apps/happyshopr/src -name "*.erl" -exec erlfmt -w {} \;
        echo "✅ Formatting complete!"
        ;;

    "docs")
        echo "📚 Generating documentation..."
        rebar3 edoc
        echo "✅ Documentation generated in doc/"
        ;;

    "docker-build")
        echo "🐳 Building Docker image..."
        docker build -t happyshopr:latest .
        echo "✅ Docker image built!"
        ;;

    "docker-run")
        echo "🐳 Running HappyShopr in Docker..."
        docker run -d -p 8080:8080 --name happyshopr happyshopr:latest
        echo "✅ HappyShopr running in Docker!"
        echo "   Access at: http://localhost:8080"
        ;;

    "backup")
        BACKUP_DIR="backups/mnesia-$(date +%Y%m%d-%H%M%S)"
        echo "💾 Creating backup..."
        mkdir -p "$BACKUP_DIR"
        cp -r data/mnesia/* "$BACKUP_DIR/" 2>/dev/null || echo "No data to backup"
        echo "✅ Backup created at $BACKUP_DIR"
        ;;

    "help"|"")
        echo "HappyShopr Development Helper"
        echo ""
        echo "Usage: ./dev.sh <command>"
        echo ""
        echo "Commands:"
        echo "  compile      - Compile the application"
        echo "  run          - Start in development mode (interactive shell)"
        echo "  release      - Build production release"
        echo "  clean        - Clean build artifacts and data"
        echo "  test         - Run tests"
        echo "  health       - Check health endpoint"
        echo "  api-test     - Run API integration tests"
        echo "  logs         - Show recent logs and crash dumps"
        echo "  format       - Format Erlang code"
        echo "  docs         - Generate documentation"
        echo "  backup       - Backup Mnesia database"
        echo "  help         - Show this help message"
        echo ""
        echo "Example:"
        echo "  ./dev.sh compile"
        echo "  ./dev.sh run"
        echo "  ./dev.sh api-test your-api-key"
        ;;

    *)
        echo "❌ Unknown command: $CMD"
        echo "Run './dev.sh help' for usage information"
        exit 1
        ;;
esac
