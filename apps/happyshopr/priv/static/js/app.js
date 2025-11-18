import { HappyShoprAPI } from './api.js';

/**
 * Application State
 */
class AppState {
    constructor() {
        this.api = null;
        this.config = this.loadConfig();
        this.currentList = null;
        this.items = [];
        this.viewMode = 'all'; // 'all' or 'recipe'
        this.hideCompleted = false;
    }

    loadConfig() {
        const saved = localStorage.getItem('happyshopr_config');
        if (saved) {
            return JSON.parse(saved);
        }
        // Auto-detect base path (e.g., /shopr)
        const path = window.location.pathname;
        const basePath = path.startsWith('/shopr') ? '/shopr' : '';
        const defaultApiUrl = window.location.origin + basePath;

        // Default config
        return {
            apiUrl: defaultApiUrl,
            apiKey: 'demo-api-key-replace-in-production',
            listId: null
        };
    }

    saveConfig() {
        localStorage.setItem('happyshopr_config', JSON.stringify(this.config));
    }

    initAPI() {
        this.api = new HappyShoprAPI(this.config.apiUrl, this.config.apiKey);
    }
}

/**
 * UI Controller
 */
class UIController {
    constructor(state) {
        this.state = state;
        this.elements = {
            loadingState: document.getElementById('loadingState'),
            errorState: document.getElementById('errorState'),
            errorMessage: document.getElementById('errorMessage'),
            viewControls: document.getElementById('viewControls'),
            itemsList: document.getElementById('itemsList'),
            addItemsContainer: document.getElementById('addItemsContainer'),
            clearCompletedContainer: document.getElementById('clearCompletedContainer'),
            completedCount: document.getElementById('completedCount'),

            // Buttons
            viewAllBtn: document.getElementById('viewAllBtn'),
            viewByRecipeBtn: document.getElementById('viewByRecipeBtn'),
            hideCompletedCheckbox: document.getElementById('hideCompletedCheckbox'),
            addItemsBtn: document.getElementById('addItemsBtn'),
            clearCompletedBtn: document.getElementById('clearCompletedBtn'),
            settingsBtn: document.getElementById('settingsBtn'),

            // Modals
            addItemsModal: document.getElementById('addItemsModal'),
            settingsModal: document.getElementById('settingsModal'),
            recipeDetailModal: document.getElementById('recipeDetailModal'),

            // Add Items Modal
            closeAddModalBtn: document.getElementById('closeAddModalBtn'),
            cancelAddBtn: document.getElementById('cancelAddBtn'),
            submitAddBtn: document.getElementById('submitAddBtn'),
            recipeNameInput: document.getElementById('recipeNameInput'),
            itemsTextarea: document.getElementById('itemsTextarea'),

            // Settings Modal
            closeSettingsBtn: document.getElementById('closeSettingsBtn'),
            cancelSettingsBtn: document.getElementById('cancelSettingsBtn'),
            saveSettingsBtn: document.getElementById('saveSettingsBtn'),
            apiUrlInput: document.getElementById('apiUrlInput'),
            apiKeyInput: document.getElementById('apiKeyInput'),
            listIdInput: document.getElementById('listIdInput'),

            // Recipe Detail Modal
            closeRecipeDetailBtn: document.getElementById('closeRecipeDetailBtn'),
            recipeDetailTitle: document.getElementById('recipeDetailTitle'),
            recipeDetailContent: document.getElementById('recipeDetailContent')
        };

        this.bindEvents();
    }

    bindEvents() {
        // View mode buttons
        this.elements.viewAllBtn.addEventListener('click', () => this.setViewMode('all'));
        this.elements.viewByRecipeBtn.addEventListener('click', () => this.setViewMode('recipe'));
        this.elements.hideCompletedCheckbox.addEventListener('change', (e) => {
            this.state.hideCompleted = e.target.checked;
            this.render();
        });

        // Add items
        this.elements.addItemsBtn.addEventListener('click', () => this.showAddItemsModal());
        this.elements.closeAddModalBtn.addEventListener('click', () => this.hideAddItemsModal());
        this.elements.cancelAddBtn.addEventListener('click', () => this.hideAddItemsModal());
        this.elements.submitAddBtn.addEventListener('click', () => this.handleAddItems());

        // Settings
        this.elements.settingsBtn.addEventListener('click', () => this.showSettingsModal());
        this.elements.closeSettingsBtn.addEventListener('click', () => this.hideSettingsModal());
        this.elements.cancelSettingsBtn.addEventListener('click', () => this.hideSettingsModal());
        this.elements.saveSettingsBtn.addEventListener('click', () => this.handleSaveSettings());

        // Clear completed
        this.elements.clearCompletedBtn.addEventListener('click', () => this.handleClearCompleted());

        // Recipe detail
        this.elements.closeRecipeDetailBtn.addEventListener('click', () => this.hideRecipeDetailModal());
    }

    setViewMode(mode) {
        this.state.viewMode = mode;

        if (mode === 'all') {
            this.elements.viewAllBtn.className = 'px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700';
            this.elements.viewByRecipeBtn.className = 'px-4 py-2 bg-gray-200 text-gray-700 rounded hover:bg-gray-300';
        } else {
            this.elements.viewAllBtn.className = 'px-4 py-2 bg-gray-200 text-gray-700 rounded hover:bg-gray-300';
            this.elements.viewByRecipeBtn.className = 'px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700';
        }

        this.render();
    }

    showLoading() {
        this.elements.loadingState.classList.remove('hidden');
        this.elements.errorState.classList.add('hidden');
        this.elements.viewControls.classList.add('hidden');
        this.elements.itemsList.classList.add('hidden');
        this.elements.addItemsContainer.classList.add('hidden');
    }

    showError(message) {
        this.elements.loadingState.classList.add('hidden');
        this.elements.errorState.classList.remove('hidden');
        this.elements.errorMessage.textContent = message;
    }

    showContent() {
        this.elements.loadingState.classList.add('hidden');
        this.elements.errorState.classList.add('hidden');
        this.elements.viewControls.classList.remove('hidden');
        this.elements.itemsList.classList.remove('hidden');
        this.elements.addItemsContainer.classList.remove('hidden');
    }

    render() {
        const filteredItems = this.state.hideCompleted
            ? this.state.items.filter(item => !item.completed)
            : this.state.items;

        if (this.state.viewMode === 'all') {
            this.renderAllItems(filteredItems);
        } else {
            this.renderByRecipe(filteredItems);
        }

        this.updateClearCompletedButton();
    }

    renderAllItems(items) {
        if (items.length === 0) {
            this.elements.itemsList.innerHTML = `
                <div class="text-center py-12 text-gray-500">
                    <p class="text-lg">No items in your list</p>
                    <p class="text-sm">Click "Add Items" to get started!</p>
                </div>
            `;
            return;
        }

        const html = items.map(item => this.renderItem(item)).join('');
        this.elements.itemsList.innerHTML = html;

        // Bind item events
        this.bindItemEvents();
    }

    renderByRecipe(items) {
        const recipeGroups = this.groupByRecipe(items);

        if (Object.keys(recipeGroups).length === 0) {
            this.elements.itemsList.innerHTML = `
                <div class="text-center py-12 text-gray-500">
                    <p class="text-lg">No items in your list</p>
                    <p class="text-sm">Click "Add Items" to get started!</p>
                </div>
            `;
            return;
        }

        let html = '';

        // General items (no recipe)
        if (recipeGroups['_general']) {
            html += this.renderRecipeGroup('General Items', recipeGroups['_general'], null);
        }

        // Recipe groups
        Object.entries(recipeGroups).forEach(([recipeId, group]) => {
            if (recipeId !== '_general') {
                html += this.renderRecipeGroup(group.name, group.items, recipeId);
            }
        });

        this.elements.itemsList.innerHTML = html;

        // Bind item events
        this.bindItemEvents();
    }

    groupByRecipe(items) {
        const groups = {};

        items.forEach(item => {
            if (!item.recipe_id) {
                if (!groups['_general']) {
                    groups['_general'] = { name: 'General Items', items: [] };
                }
                groups['_general'].items.push(item);
            } else {
                if (!groups[item.recipe_id]) {
                    groups[item.recipe_id] = {
                        name: item.recipe_name || 'Unknown Recipe',
                        items: []
                    };
                }
                groups[item.recipe_id].items.push(item);
            }
        });

        return groups;
    }

    renderRecipeGroup(name, items, recipeId) {
        const requiredItems = items.filter(i => i.required);
        const completedItems = requiredItems.filter(i => i.completed);
        const progress = requiredItems.length > 0
            ? `${completedItems.length}/${requiredItems.length}`
            : '0/0';

        return `
            <div class="recipe-group bg-white rounded-lg shadow p-4 mb-4">
                <div class="flex justify-between items-center mb-3">
                    <h3 class="text-lg font-semibold">${this.escapeHtml(name)}</h3>
                    <span class="text-sm bg-blue-100 text-blue-800 px-2 py-1 rounded">[${progress}]</span>
                </div>
                <div class="space-y-2">
                    ${items.map(item => this.renderItem(item)).join('')}
                </div>
            </div>
        `;
    }

    renderItem(item) {
        const completedClass = item.completed ? 'item-completed' : '';
        const notRequiredClass = !item.required ? 'item-not-required' : '';
        const checkbox = item.completed ? '✓' : '○';

        const quantityText = item.quantity ? ` <span class="text-gray-600">${this.escapeHtml(item.quantity)}</span>` : '';
        const notRequiredBadge = !item.required ? ' <span class="text-xs bg-gray-200 px-2 py-1 rounded">NOT NEEDED</span>' : '';

        return `
            <div class="flex items-center gap-3 p-2 hover:bg-gray-50 rounded ${completedClass} ${notRequiredClass}"
                 data-item-id="${item.id}">
                <button class="toggle-complete text-2xl" data-item-id="${item.id}">
                    ${checkbox}
                </button>
                <div class="flex-1">
                    <span class="font-medium">${this.escapeHtml(item.name)}</span>${quantityText}${notRequiredBadge}
                    ${item.notes ? `<div class="text-sm text-gray-500">${this.escapeHtml(item.notes)}</div>` : ''}
                </div>
                <button class="delete-item text-red-500 hover:text-red-700" data-item-id="${item.id}">
                    🗑️
                </button>
            </div>
        `;
    }

    bindItemEvents() {
        // Toggle complete
        document.querySelectorAll('.toggle-complete').forEach(btn => {
            btn.addEventListener('click', async (e) => {
                const itemId = e.target.dataset.itemId;
                await this.handleToggleComplete(itemId);
            });
        });

        // Delete item
        document.querySelectorAll('.delete-item').forEach(btn => {
            btn.addEventListener('click', async (e) => {
                const itemId = e.target.dataset.itemId;
                if (confirm('Delete this item?')) {
                    await this.handleDeleteItem(itemId);
                }
            });
        });
    }

    async handleToggleComplete(itemId) {
        const item = this.state.items.find(i => i.id === itemId);
        if (!item) return;

        try {
            await this.state.api.markCompleted(
                this.state.currentList.id,
                itemId,
                !item.completed
            );

            // Update local state
            item.completed = !item.completed;
            this.render();
        } catch (error) {
            this.showError(`Failed to update item: ${error.message}`);
        }
    }

    async handleDeleteItem(itemId) {
        try {
            await this.state.api.deleteItem(this.state.currentList.id, itemId);

            // Remove from local state
            this.state.items = this.state.items.filter(i => i.id !== itemId);
            this.render();
        } catch (error) {
            this.showError(`Failed to delete item: ${error.message}`);
        }
    }

    async handleClearCompleted() {
        const completedCount = this.state.items.filter(i => i.completed).length;

        if (completedCount === 0) {
            alert('No completed items to clear');
            return;
        }

        if (!confirm(`Permanently delete ${completedCount} completed items?`)) {
            return;
        }

        try {
            await this.state.api.clearCompleted(this.state.currentList.id);

            // Remove from local state
            this.state.items = this.state.items.filter(i => !i.completed);
            this.render();
        } catch (error) {
            this.showError(`Failed to clear completed items: ${error.message}`);
        }
    }

    updateClearCompletedButton() {
        const completedCount = this.state.items.filter(i => i.completed).length;
        this.elements.completedCount.textContent = completedCount;

        if (completedCount > 0) {
            this.elements.clearCompletedContainer.classList.remove('hidden');
        } else {
            this.elements.clearCompletedContainer.classList.add('hidden');
        }
    }

    // Modal methods
    showAddItemsModal() {
        this.elements.addItemsModal.classList.add('active');
        this.elements.itemsTextarea.focus();
    }

    hideAddItemsModal() {
        this.elements.addItemsModal.classList.remove('active');
        this.elements.recipeNameInput.value = '';
        this.elements.itemsTextarea.value = '';
    }

    showSettingsModal() {
        this.elements.apiUrlInput.value = this.state.config.apiUrl;
        this.elements.apiKeyInput.value = this.state.config.apiKey;
        this.elements.listIdInput.value = this.state.config.listId || '';
        this.elements.settingsModal.classList.add('active');
    }

    hideSettingsModal() {
        this.elements.settingsModal.classList.remove('active');
    }

    hideRecipeDetailModal() {
        this.elements.recipeDetailModal.classList.remove('active');
    }

    async handleAddItems() {
        const recipeName = this.elements.recipeNameInput.value.trim();
        const itemsText = this.elements.itemsTextarea.value.trim();

        if (!itemsText) {
            alert('Please enter at least one item');
            return;
        }

        const items = this.parseItemsText(itemsText);

        try {
            const recipeId = recipeName ? this.generateUUID() : null;

            await this.state.api.addItems(
                this.state.currentList.id,
                items,
                recipeId,
                recipeName || null
            );

            this.hideAddItemsModal();
            await this.loadList();
        } catch (error) {
            this.showError(`Failed to add items: ${error.message}`);
        }
    }

    parseItemsText(text) {
        const lines = text.split('\n').filter(line => line.trim());

        return lines.map(line => {
            // Try to parse "Name Quantity" format
            const parts = line.trim().split(/\s+/);

            if (parts.length === 1) {
                return { name: parts[0] };
            }

            // Check if last part looks like a quantity (contains numbers)
            const lastPart = parts[parts.length - 1];
            if (/\d/.test(lastPart)) {
                const name = parts.slice(0, -1).join(' ');
                return { name, quantity: lastPart };
            }

            // Otherwise, everything is the name
            return { name: line.trim() };
        });
    }

    async handleSaveSettings() {
        this.state.config.apiUrl = this.elements.apiUrlInput.value.trim();
        this.state.config.apiKey = this.elements.apiKeyInput.value.trim();
        this.state.config.listId = this.elements.listIdInput.value.trim() || null;

        this.state.saveConfig();
        this.state.initAPI();

        this.hideSettingsModal();

        // Reload the app
        await this.loadList();
    }

    async loadList() {
        this.showLoading();

        try {
            // If no list ID, create a default one
            if (!this.state.config.listId) {
                const result = await this.state.api.createList('My Shopping List');
                this.state.config.listId = result.id;
                this.state.saveConfig();
            }

            // Load the list
            const list = await this.state.api.getList(this.state.config.listId);
            this.state.currentList = list;
            this.state.items = list.items || [];

            this.showContent();
            this.render();
        } catch (error) {
            this.showError(`Failed to load list: ${error.message}. Click Settings (⚙️) to configure.`);
        }
    }

    generateUUID() {
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            const r = Math.random() * 16 | 0;
            const v = c === 'x' ? r : (r & 0x3 | 0x8);
            return v.toString(16);
        });
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
}

/**
 * Application initialization
 */
async function init() {
    const state = new AppState();
    state.initAPI();

    const ui = new UIController(state);

    // Load initial data
    await ui.loadList();
}

// Start the app
init();
