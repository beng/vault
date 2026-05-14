# TypeScript / Frontend Guidelines

> These guidelines are mandatory for all TypeScript code in this project.

## Mandatory Code Patterns (CRITICAL - Follow First)

### 1. Functional Programming Only

- Prefer factory functions and plain objects for core business logic
- Do not introduce classes for core runtime/feature logic unless explicitly requested
- Classes are acceptable for: repositories, framework requirements (NestJS controllers), services needing true encapsulation

```typescript
// ✓ Factory function (preferred for core logic)
export function createRuntime(config: RuntimeConfig): Runtime {
  // ...
}

// ✗ FORBIDDEN for core runtime logic
export class Runtime {
  constructor(config: RuntimeConfig) {
    /* ... */
  }
}

// ✓ ACCEPTABLE - repository (data access layer)
export class UserRepository implements IUserRepository {
  constructor(private readonly db: Database) {}
  async findById(id: string): Promise<User | null> { ... }
}
```

### 2. Feature-Based File Structure

For each feature:

```
{feature}/
├── index.ts       # Barrel exports ONLY
├── types.ts       # Types UNIQUE to this feature
├── core.ts        # Business logic / pure functions
└── __tests__/     # Tests
```

Rules:

- `index.ts`: only re-exports from sibling files (types.ts, core.ts, etc.)
- `types.ts`: interfaces and types unique to this feature only
- `core.ts`: pure / business logic; no framework wiring
- `__tests__/`: colocated tests (Vitest/Jest), mirroring core.ts exports

### 3. NO Stutter Imports (FORBIDDEN)

Never import X/X when X has an index.ts.

```typescript
// ✗ FORBIDDEN
import { Router } from "router/router";

// ✓ CORRECT
import { Router } from "router"; // resolves to router/index.ts
```

### 4. File Naming

- React components: PascalCase.tsx
- All other source files: camelCase.ts

### 5. Logging Pattern

Always log with a clear component/module prefix and structured context:

```typescript
console.log("[ModuleName] Action description", { id, state, payload });
console.error("[ModuleName] Operation failed", { error, context });
```

- Prefer structured objects as the last argument
- Log state transitions, key decisions, and failures

### 6. Error Handling - Result Pattern for HTTP

For HTTP / transport logic, use a Result<T, E>-style pattern instead of throwing:

```typescript
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

// ✓ Preferred
async function fetchData(id: string): Promise<Result<Data, FetchError>> {
  try {
    const res = await http.get(`api/data/${id}`);
    return { ok: true, value: res.data };
  } catch (error) {
    console.error("[Http] fetchData error", { error, id });
    return { ok: false, error };
  }
}
```

- Do not throw from HTTP helpers - surface errors as Result and handle them at the call site

### 7. Complete Code - No Omissions

When generating code in chat:

- Always show full file contents
- Never use placeholders like:
  - `// ... rest of code`
  - `// ... existing code`
  - `// ... other methods`

---

## Domain & Type Architecture

### 8. Domain Boundary Rule

Types live where their domain lives. A feature module only defines types unique to that feature.

```typescript
// ✗ BAD - timeline defining diagnostic types
// timeline/types.ts
interface Diagnostic { ... } // Diagnostics is its own domain!

// ✓ GOOD - import from source
// timeline/types.ts
import type { Diagnostic } from '../diagnostics';

interface FeedbackEvent {
  type: 'feedback';
  diagnostics: Diagnostic[]; // Reference, don't redefine
}
```

### 9. Search Before Create

Before defining ANY type:

1. Grep the codebase for existing definitions
2. If it exists, import it
3. If it doesn't exist, ask: does this belong HERE or in a shared module?

```typescript
// Before writing: interface GridCell { x: number; y: number }
// Search: grep -r "interface GridCell" src/
// Found in: src/layout/types.ts
// Action: import { GridCell } from '../layout'
```

### 10. Minimal Type Surface

A module's types.ts defines ONLY types unique to that module.

```typescript
// ✓ GOOD - feature-specific types only
// panel/types.ts
export interface PanelProps {
  isExpanded: boolean;
  onToggle: () => void;
  children: React.ReactNode;
}

// ✗ BAD - generic types that belong elsewhere
// panel/types.ts
export interface Position { x: number; y: number } // Too generic!
export interface User { id: string; name: string } // Not panel-specific!
```

### 11. Discriminated Unions for Variants

Use discriminated unions so TypeScript can narrow types automatically:

```typescript
// ✓ GOOD - discriminated union
type Event =
  | { type: 'created'; payload: CreatePayload }
  | { type: 'updated'; payload: UpdatePayload; previousValue: string }
  | { type: 'deleted'; id: string };

function handleEvent(event: Event) {
  switch (event.type) {
    case 'created':
      // TS knows: event.payload is CreatePayload
      break;
    case 'updated':
      // TS knows: event.payload is UpdatePayload, event.previousValue exists
      break;
    case 'deleted':
      // TS knows: event.id exists, no payload
      break;
  }
}
```

### 12. Avoid Enums

Use union types or const objects instead:

```typescript
// ✗ AVOID
enum Status {
  Pending,
  Active,
  Completed,
}

// ✓ PREFERRED - union type
type Status = 'pending' | 'active' | 'completed';

// ✓ ALSO GOOD - const object when you need runtime values
const STATUS = {
  PENDING: 'pending',
  ACTIVE: 'active',
  COMPLETED: 'completed',
} as const;
type Status = (typeof STATUS)[keyof typeof STATUS];
```

### 13. Type Inference

Let TypeScript infer when obvious. Annotate public APIs explicitly.

```typescript
// ✓ Let TS infer local variables
const count = 0;
const items = [1, 2, 3];
const user = { name: 'Ben', age: 30 };

// ✗ Over-annotated
const count: number = 0;
const items: number[] = [1, 2, 3];

// ✓ DO annotate public APIs
export function processItems(items: Item[]): ProcessedItem[] {
  // ...
}

// ✓ DO annotate when inference isn't obvious
const handlers: Record<string, Handler> = {};
```

### 14. Use Built-in Utility Types

```typescript
// ✓ Use Pick, Omit, Partial, Required
type UserPreview = Pick<User, 'id' | 'name'>;
type UserWithoutPassword = Omit<User, 'password'>;
type PartialConfig = Partial<Config>;
type RequiredOptions = Required<Options>;

// ✗ Don't manually redeclare
interface UserPreview {
  id: string;
  name: string;
}
```

---

## Project Structure (Comprehensive)

```
src/
├── index.ts                # Entry point
├── config.ts               # Configuration
├── types/                  # Shared type definitions
│   ├── index.ts
│   └── user.ts
├── models/                 # Data models / entities
│   └── user.model.ts
├── services/               # Business logic
│   └── user.service.ts
├── repositories/           # Data access
│   └── user.repository.ts
├── api/                    # API layer
│   ├── routes/
│   │   └── user.routes.ts
│   ├── middleware/
│   │   └── auth.middleware.ts
│   └── controllers/
│       └── user.controller.ts
├── utils/                  # Shared utilities
│   └── helpers.ts
└── errors/                 # Custom errors
    └── index.ts

tests/
├── setup.ts                # Test setup
├── factories/              # Test data factories
│   └── user.factory.ts
├── unit/
│   └── user.service.test.ts
├── integration/
│   └── user.api.test.ts
└── e2e/
    └── user.flow.test.ts
```

---

## Code Style

### Formatting
- Use Prettier with default settings
- Line length: 100 characters
- Single quotes for strings
- No semicolons (or always semicolons - pick one, be consistent)
- Run `prettier --write .` before committing

### ESLint Rules
```json
{
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:@typescript-eslint/recommended-requiring-type-checking"
  ],
  "rules": {
    "@typescript-eslint/explicit-function-return-type": "error",
    "@typescript-eslint/no-explicit-any": "error",
    "@typescript-eslint/no-unused-vars": "error",
    "@typescript-eslint/strict-boolean-expressions": "error"
  }
}
```

---

## Type Definitions

### Prefer Interfaces for Objects
```typescript
// ✅ Good - interface for object shapes
interface User {
  id: string
  email: string
  name: string
  createdAt: Date
}

// ✅ Good - type for unions, primitives, utility types
type UserRole = 'admin' | 'user' | 'guest'
type UserId = string
type UserWithRole = User & { role: UserRole }

// ❌ Bad - type for simple object (use interface)
type User = {
  id: string
  email: string
}
```

### Strict Typing (No `any`)
```typescript
// ✅ Good - explicit types
function processData(data: unknown): ProcessedData {
  if (!isValidData(data)) {
    throw new ValidationError('Invalid data')
  }
  return transform(data)
}

// ✅ Good - generic when type varies
function firstElement<T>(arr: T[]): T | undefined {
  return arr[0]
}

// ❌ Bad - using any
function processData(data: any): any {
  return data.something
}
```

### Readonly and Immutability
```typescript
// ✅ Good - immutable by default
interface Config {
  readonly apiUrl: string
  readonly timeout: number
  readonly features: readonly string[]
}

// ✅ Good - const assertions
const ROLES = ['admin', 'user', 'guest'] as const
type Role = typeof ROLES[number]  // 'admin' | 'user' | 'guest'

// ✅ Good - Readonly utility
function processUsers(users: ReadonlyArray<User>): void {
  // users.push() would error - enforces immutability
}
```

---

## Functions

### Explicit Return Types
```typescript
// ✅ Good - explicit return types (always)
function getUser(id: string): User | null {
  return userRepo.findById(id)
}

async function fetchUsers(): Promise<User[]> {
  return api.get('/users')
}

// ❌ Bad - inferred return type
function getUser(id: string) {  // Return type not explicit
  return userRepo.findById(id)
}
```

### Function Overloads
```typescript
// ✅ Good - overloads for different signatures
function createElement(tag: 'div'): HTMLDivElement
function createElement(tag: 'span'): HTMLSpanElement
function createElement(tag: string): HTMLElement
function createElement(tag: string): HTMLElement {
  return document.createElement(tag)
}
```

### Arrow Functions vs Function Declarations
```typescript
// Use function declarations for named exports
export function calculateTotal(items: Item[]): number {
  return items.reduce((sum, item) => sum + item.price, 0)
}

// Use arrow functions for callbacks and inline
const doubled = numbers.map((n) => n * 2)

// Use arrow functions for class properties (preserves `this`)
class Handler {
  private count = 0
  
  handleClick = (): void => {
    this.count++  // `this` is correct
  }
}
```

---

## Classes and OOP

**Note:** Prefer functional patterns for core business logic. Use classes primarily for:
- Repositories (data access layer)
- Services that truly need encapsulation
- Framework requirements (e.g., NestJS controllers)

For pure logic, prefer functions + data structures.

```typescript
interface IUserRepository {
  findById(id: string): Promise<User | null>
  create(data: CreateUserInput): Promise<User>
  update(id: string, data: UpdateUserInput): Promise<User>
  delete(id: string): Promise<void>
}

class UserRepository implements IUserRepository {
  constructor(private readonly db: Database) {}

  async findById(id: string): Promise<User | null> {
    const row = await this.db.query('SELECT * FROM users WHERE id = ?', [id])
    return row ? this.mapToUser(row) : null
  }

  async create(data: CreateUserInput): Promise<User> {
    const id = generateId()
    await this.db.insert('users', { id, ...data })
    return this.findById(id) as Promise<User>
  }

  async update(id: string, data: UpdateUserInput): Promise<User> {
    await this.db.update('users', data, { id })
    const user = await this.findById(id)
    if (!user) throw new NotFoundError(`User ${id} not found`)
    return user
  }

  async delete(id: string): Promise<void> {
    await this.db.delete('users', { id })
  }

  private mapToUser(row: DbRow): User {
    return {
      id: row.id,
      email: row.email,
      name: row.name,
      createdAt: new Date(row.created_at),
    }
  }
}
```

---

## Error Handling

### Custom Error Classes
```typescript
// src/errors/index.ts

export class AppError extends Error {
  constructor(
    message: string,
    public readonly code: string,
    public readonly statusCode: number = 500,
  ) {
    super(message)
    this.name = this.constructor.name
    Error.captureStackTrace(this, this.constructor)
  }
}

export class NotFoundError extends AppError {
  constructor(resource: string) {
    super(`${resource} not found`, 'NOT_FOUND', 404)
  }
}

export class ValidationError extends AppError {
  constructor(
    message: string,
    public readonly fields?: Record<string, string>,
  ) {
    super(message, 'VALIDATION_ERROR', 400)
  }
}

export class UnauthorizedError extends AppError {
  constructor(message = 'Unauthorized') {
    super(message, 'UNAUTHORIZED', 401)
  }
}
```

### Error Handling Pattern
```typescript
// ✅ Good - typed errors, proper propagation
async function getUser(id: string): Promise<User> {
  try {
    const user = await repository.findById(id)
    if (!user) {
      throw new NotFoundError(`User ${id}`)
    }
    return user
  } catch (error) {
    if (error instanceof AppError) {
      throw error  // Re-throw known errors
    }
    logger.error('Unexpected error fetching user', { id, error })
    throw new AppError('Failed to fetch user', 'INTERNAL_ERROR')
  }
}

// ❌ Bad - swallowing errors
async function getUser(id: string): Promise<User | null> {
  try {
    return await repository.findById(id)
  } catch {
    return null  // Lost error context!
  }
}
```

---

## Async/Await

```typescript
// ✅ Good - parallel execution when independent
async function loadDashboard(userId: string): Promise<Dashboard> {
  const [user, posts, notifications] = await Promise.all([
    userService.getUser(userId),
    postService.getUserPosts(userId),
    notificationService.getUnread(userId),
  ])
  
  return { user, posts, notifications }
}

// ✅ Good - sequential when dependent
async function createUserWithProfile(data: CreateUserInput): Promise<User> {
  const user = await userService.create(data)
  await profileService.createDefault(user.id)
  await emailService.sendWelcome(user.email)
  return user
}

// ❌ Bad - unnecessary sequential
async function loadData(): Promise<void> {
  const users = await fetchUsers()     // Waits
  const posts = await fetchPosts()     // Then waits - but these are independent!
  const comments = await fetchComments()
}
```

---

## Null Handling

```typescript
// ✅ Good - explicit null checks
function getDisplayName(user: User | null): string {
  if (user === null) {
    return 'Anonymous'
  }
  return user.name ?? user.email
}

// ✅ Good - optional chaining
const city = user?.address?.city ?? 'Unknown'

// ✅ Good - nullish coalescing
const pageSize = config.pageSize ?? 20

// ❌ Bad - using || for defaults (falsy vs nullish)
const count = input || 10  // Bug: input of 0 becomes 10!

// ✅ Correct
const count = input ?? 10  // Only null/undefined becomes 10
```

---

## Type Guards

```typescript
// Custom type guard
function isUser(value: unknown): value is User {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'email' in value &&
    typeof (value as User).id === 'string' &&
    typeof (value as User).email === 'string'
  )
}

// Using type guards
function processEntity(entity: User | Post): void {
  if (isUser(entity)) {
    console.log(entity.email)  // TypeScript knows it's User
  } else {
    console.log(entity.title)  // TypeScript knows it's Post
  }
}

// Discriminated unions
interface SuccessResult {
  success: true
  data: User
}

interface ErrorResult {
  success: false
  error: string
}

type Result = SuccessResult | ErrorResult

function handleResult(result: Result): void {
  if (result.success) {
    console.log(result.data.name)  // Narrowed to SuccessResult
  } else {
    console.log(result.error)  // Narrowed to ErrorResult
  }
}
```

---

## Zod for Validation

```typescript
import { z } from 'zod'

// Define schema
const CreateUserSchema = z.object({
  email: z.string().email(),
  name: z.string().min(1).max(100),
  age: z.number().int().min(0).max(150).optional(),
  role: z.enum(['admin', 'user', 'guest']).default('user'),
})

// Infer types from schema
type CreateUserInput = z.infer<typeof CreateUserSchema>

// Validate
function createUser(input: unknown): User {
  const validated = CreateUserSchema.parse(input)  // Throws if invalid
  return userRepository.create(validated)
}

// Safe parse (no throw)
function tryCreateUser(input: unknown): Result<User> {
  const result = CreateUserSchema.safeParse(input)
  if (!result.success) {
    return { success: false, error: result.error.message }
  }
  return { success: true, data: userRepository.create(result.data) }
}
```

---

## Module Organization

```typescript
// ✅ Good - named exports
// user.service.ts
export function createUser(data: CreateUserInput): Promise<User> { ... }
export function getUser(id: string): Promise<User | null> { ... }
export function updateUser(id: string, data: UpdateUserInput): Promise<User> { ... }

// ✅ Good - barrel exports
// services/index.ts
export * from './user.service'
export * from './post.service'
export * from './auth.service'

// ❌ Avoid default exports (harder to refactor, inconsistent naming)
export default class UserService { }
```

---

## Documentation

```typescript
/**
 * Calculates the total price including tax and discounts.
 *
 * @param items - Array of items to calculate
 * @param taxRate - Tax rate as decimal (e.g., 0.08 for 8%)
 * @param discountCode - Optional discount code to apply
 * @returns Total price in cents
 * @throws {ValidationError} If items array is empty
 *
 * @example
 * ```typescript
 * const total = calculateTotal(
 *   [{ price: 1000 }, { price: 2000 }],
 *   0.08,
 *   'SAVE10'
 * )
 * // Returns: 2916 (3000 - 10% + 8% tax)
 * ```
 */
export function calculateTotal(
  items: Item[],
  taxRate: number,
  discountCode?: string,
): number {
  if (items.length === 0) {
    throw new ValidationError('Items array cannot be empty')
  }
  // ...
}
```

---

## Forbidden Patterns

These are **NEVER allowed** unless explicitly requested:

- ❌ **Classes for core logic** - Use functions + data structures
- ❌ **Singleton stores** - Prefer per-instance factories
- ❌ **Stutter imports** (feature/feature) - Use barrel exports
- ❌ **Throwing from HTTP helpers** - Use Result<T, E> pattern
- ❌ **Enums** - Use union types or const objects
- ❌ **Defining domain types outside their domain module**
- ❌ **Using `any`** - Use `unknown` with type guards
- ❌ **Non-null assertion without certainty** - Use optional chaining
- ❌ **Type assertions without validation** - Validate first
- ❌ **Nested callbacks** - Use async/await
- ❌ **Mutating function parameters** - Return new values

---

## Required Tools

```bash
# Install dev dependencies
npm install -D typescript @types/node eslint prettier
npm install -D @typescript-eslint/parser @typescript-eslint/eslint-plugin

# Validate before commit
npx prettier --check .
npx eslint .
npx tsc --noEmit
npm test
```

---

## Pre-Commit Checklist

- [ ] All functions have explicit return types
- [ ] No `any` types (use `unknown` + type guards)
- [ ] Functional patterns for core logic (no classes unless justified)
- [ ] No stutter imports (feature/feature)
- [ ] HTTP errors use Result pattern (don't throw)
- [ ] Types defined in their domain module
- [ ] No enums (use unions)
- [ ] `prettier --check .` passes
- [ ] `eslint .` shows no errors
- [ ] `tsc --noEmit` compiles without errors
- [ ] All tests pass
- [ ] JSDoc on public functions
