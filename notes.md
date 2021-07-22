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
  
    `(user: UserId, id: PlanId) => ???`
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

- `POST /:user/:id/title/:title`
  Sets the name of a plan (save/save as if name changes)
  
    `(user: UserId, id: PlanId, title: String) => PlanId`

- `POST /:user/:id/copy`
  Copies plan as new untitled plan
  
    `(userId: UserId, id: PlanId) => PlanId`
- `GET /:user/plans?PageQuery`
  Gets a user's plans
  
    `(user: UserId) => Page[???]`

##### TODO `PageQuery`, `Page`

#### Compare API

**TODO**
