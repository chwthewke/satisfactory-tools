### API Notes

#### Session API

- `POST /new`
  Create a new session with credentials

    `Credentials => (UserId, SessionId)`
- `GET /:sess`
  Get the user id for a session
  
    `SessionId => Option[UserId]`


#### Plan API

- `POST /:user/new` Create new plan

    `(user: UserId) => PlanId`
- `POST /:user/:id/group/add`
  Add empty custom group to plan (if below limit)

    `(user: UserId, id: PlanId) => Boolean`
- `POST /:user/:id/group/remove`
  Remove last custom group from plan (if empty)

    `(user: UserId, id: PlanId) => Boolean`
- `PUT /:user/:id/bill`
  Set the bill for a plan

    `(user: UserId, id: PlanId, body: Bill) => Unit`
- `PUT /:user/:id/recipes`
  Set the allowed recipes for a plan

    `(user: UserId, id: PlanId, body: RecipeList) => Unit`
- `PUT /:user/:id/options`
  Set the factory options for a plan

    `(user: UserId, id: PlanId, body: Options) => Unit`
- `PUT /:user/:id/resources`
  Set the resource options for a plan

    `(user: UserId, id: PlanId, body: ResourceOptions) => Unit`
- `PUT /:user/:id/groups`
  Set the custom group selection for a plan

    `(user: UserId, id: PlanId, body: CustomGroupSelection) => Unit`
- `GET /:user/:id/header`
  Get the header data for a plan
  
    `(user: UserId, id: PlanId) => PlanHeader`
- `GET /:user/:id/:input`
  Get a part of the input state for a plan

    `(user: UserId, id: PlanId, input: InputTab) => input.Data`
- `GET /:user/:id/:output`
  Get a part of the output state for a plan

    `(user: UserId, id: PlanId, output: OutputTab) => output.Data`

- `POST /:user/:id/compute`
  Compute a plan's solution
  
    `(user: UserId, id: PlanId) => Unit`

#### Library API

- `POST /:id/title/:title`
  Sets the name of a plan (save/save as if name changes)
  
    `(id: PlanId, title: String) => PlanId`

- `POST /:id/copy/soft`
  Copies plan as new untitled plan, with a reference to the original for saving
  
    `(id: PlanId) => PlanId`
- `POST /:id/copy/hard`
  Copies plan as new copied plan
  
    `(userId: UserId, id: PlanId) => PlanId`
- `GET /:user/plans?PageQuery`
  Gets a user's plans
  
    `(user: UserId) => Page[???]`
- `GET /:user/plans`
  Gets all a user's plans
  
    `(user: UserId) => Page[???]`

##### Plan state transitions

`POST /new`  
&#x21D2; new plan, `src_id = NULL`, `name = NULL` (untitled plan)

`POST /:id/title`  
&#x21D2; if name exists for user, overwrite existing plan  
&#x21D2; otherwise set name

`POST /:id/edit` (soft copy)  
&#x21D2; new plan, `src_id = id`, `name = NULL` (name from src plan), copy all params

`POST /:id/copy` (hard copy)  
&#x21D2; new plan, `src_id = NULL`, `name = '$name (copy)'`, copy all params




#### Compare API

**TODO**
