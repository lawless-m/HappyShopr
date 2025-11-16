/**
 * HappyShopr API Client
 */
export class HappyShoprAPI {
    constructor(baseUrl, apiKey) {
        this.baseUrl = baseUrl.replace(/\/$/, ''); // Remove trailing slash
        this.apiKey = apiKey;
    }

    /**
     * Make authenticated API request
     */
    async request(method, path, body = null) {
        const url = `${this.baseUrl}${path}`;
        const headers = {
            'Authorization': `Bearer ${this.apiKey}`,
            'Content-Type': 'application/json'
        };

        const options = {
            method,
            headers
        };

        if (body) {
            options.body = JSON.stringify(body);
        }

        try {
            const response = await fetch(url, options);

            if (!response.ok) {
                const errorData = await response.json().catch(() => ({}));
                throw new Error(errorData.error?.message || `HTTP ${response.status}`);
            }

            return await response.json();
        } catch (error) {
            console.error('API request failed:', error);
            throw error;
        }
    }

    /**
     * Health check
     */
    async health() {
        const response = await fetch(`${this.baseUrl}/health`);
        return await response.json();
    }

    /**
     * Get all lists
     */
    async getLists() {
        return this.request('GET', '/api/v1/lists');
    }

    /**
     * Get single list with items
     */
    async getList(listId) {
        return this.request('GET', `/api/v1/lists/${listId}`);
    }

    /**
     * Create new list
     */
    async createList(name) {
        return this.request('POST', '/api/v1/lists', { name });
    }

    /**
     * Update list
     */
    async updateList(listId, updates) {
        return this.request('PUT', `/api/v1/lists/${listId}`, updates);
    }

    /**
     * Delete list
     */
    async deleteList(listId) {
        return this.request('DELETE', `/api/v1/lists/${listId}`);
    }

    /**
     * Add items to list
     * @param {string} listId
     * @param {Array} items - Array of {name, quantity?, notes?}
     * @param {string} recipeId - Optional recipe ID
     * @param {string} recipeName - Optional recipe name
     */
    async addItems(listId, items, recipeId = null, recipeName = null) {
        const body = { items };

        if (recipeId) {
            body.recipe_id = recipeId;
            body.recipe_name = recipeName;
        }

        return this.request('POST', `/api/v1/lists/${listId}/items`, body);
    }

    /**
     * Update item
     */
    async updateItem(listId, itemId, updates) {
        return this.request('PUT', `/api/v1/lists/${listId}/items/${itemId}`, updates);
    }

    /**
     * Delete item
     */
    async deleteItem(listId, itemId) {
        return this.request('DELETE', `/api/v1/lists/${listId}/items/${itemId}`);
    }

    /**
     * Mark item as completed/uncompleted
     */
    async markCompleted(listId, itemId, completed = true) {
        return this.request('POST', `/api/v1/lists/${listId}/items/${itemId}/complete`, { completed });
    }

    /**
     * Toggle item required status
     */
    async toggleRequired(listId, itemId, required) {
        return this.request('POST', `/api/v1/lists/${listId}/items/${itemId}/required`, { required });
    }

    /**
     * Clear all completed items (permanent deletion)
     */
    async clearCompleted(listId) {
        return this.request('DELETE', `/api/v1/lists/${listId}/items/completed`);
    }

    /**
     * Get recipes summary
     */
    async getRecipes(listId) {
        return this.request('GET', `/api/v1/lists/${listId}/recipes`);
    }

    /**
     * Get items for specific recipe
     */
    async getRecipeItems(listId, recipeId) {
        return this.request('GET', `/api/v1/lists/${listId}/recipes/${recipeId}/items`);
    }
}
