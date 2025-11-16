%% Record definitions for HappyShopr

-record(shopping_list, {
    id :: binary(),
    user_id :: binary(),
    name :: binary(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(shopping_item, {
    id :: binary(),
    list_id :: binary(),
    name :: binary(),
    quantity :: binary() | undefined,
    notes :: binary() | undefined,
    category :: binary() | undefined,
    recipe_id :: binary() | undefined,
    recipe_name :: binary() | undefined,
    required :: boolean(),
    completed :: boolean(),
    created_at :: integer(),
    added_by :: binary()
}).

-record(api_key, {
    key :: binary(),
    user_id :: binary(),
    description :: binary(),
    created_at :: integer(),
    last_used :: integer() | undefined
}).

-record(recipe, {
    id :: binary(),
    user_id :: binary(),
    name :: binary(),
    source :: binary() | undefined,
    created_at :: integer(),
    last_used :: integer() | undefined
}).

-record(user, {
    id :: binary(),
    email :: binary(),
    created_at :: integer()
}).
