# Unit Testing Guidelines

> These guidelines are mandatory for all unit tests in this project.

## Philosophy

1. **Tests are documentation** - A new developer should understand the code by reading tests
2. **Test behavior, not implementation** - Tests should survive refactoring
3. **One assertion per concept** - Each test verifies one specific behavior
4. **Fast and isolated** - Unit tests run in milliseconds, no external dependencies

## Test Structure

### File Organization
```
tests/
├── unit/                       # Unit tests (isolated, fast)
│   ├── services/
│   │   └── user.service.test.ts
│   ├── utils/
│   │   └── helpers.test.ts
│   └── models/
│       └── user.test.ts
├── integration/                # Tests with real dependencies
│   └── api/
│       └── user.api.test.ts
├── e2e/                        # End-to-end flows
│   └── user.flow.test.ts
├── fixtures/                   # Shared test data
│   └── users.ts
├── factories/                  # Test data factories
│   └── user.factory.ts
├── mocks/                      # Mock implementations
│   └── user.repository.mock.ts
└── setup.ts                    # Global test setup
```

### Naming Convention
```typescript
// File: [module].test.ts or [module].spec.ts
// user.service.test.ts

// Test suites: describe the module/class
describe('UserService', () => {
  
  // Nested suites: describe the method/function
  describe('createUser', () => {
    
    // Test cases: describe the behavior
    it('should create a user with valid input', () => {})
    it('should throw ValidationError when email is invalid', () => {})
    it('should hash the password before storing', () => {})
  })
})
```

## Writing Tests

### The AAA Pattern (Arrange, Act, Assert)
```typescript
it('should calculate total with tax', () => {
  // Arrange - set up test data and dependencies
  const items = [
    { name: 'Widget', price: 1000 },
    { name: 'Gadget', price: 2000 },
  ]
  const taxRate = 0.08

  // Act - execute the code under test
  const total = calculateTotal(items, taxRate)

  // Assert - verify the result
  expect(total).toBe(3240)  // 3000 + 8% tax
})
```

### Given-When-Then (Alternative for complex scenarios)
```typescript
it('should apply discount code for first-time users', () => {
  // Given a new user with no previous orders
  const user = createUser({ isFirstOrder: true })
  const cart = createCart({ items: [{ price: 10000 }] })
  
  // When they apply the WELCOME10 discount code
  const result = applyDiscount(cart, 'WELCOME10', user)
  
  // Then they receive a 10% discount
  expect(result.discount).toBe(1000)
  expect(result.total).toBe(9000)
})
```

### Test Data Factories
```typescript
// factories/user.factory.ts
interface UserOverrides {
  id?: string
  email?: string
  name?: string
  role?: UserRole
}

export function createTestUser(overrides: UserOverrides = {}): User {
  return {
    id: overrides.id ?? `user-${randomUUID()}`,
    email: overrides.email ?? `test-${randomUUID()}@example.com`,
    name: overrides.name ?? 'Test User',
    role: overrides.role ?? 'user',
    createdAt: new Date(),
  }
}

// Usage in tests
it('should allow admins to delete users', () => {
  const admin = createTestUser({ role: 'admin' })
  const targetUser = createTestUser()
  
  const result = userService.delete(targetUser.id, admin)
  
  expect(result.success).toBe(true)
})
```

## Mocking

### Mock External Dependencies (Not Internal Logic)
```typescript
// ✅ Good - mock external boundary (database, API)
const mockRepository = {
  findById: vi.fn(),
  create: vi.fn(),
  update: vi.fn(),
}

const userService = new UserService(mockRepository)

it('should return null when user not found', async () => {
  mockRepository.findById.mockResolvedValue(null)
  
  const result = await userService.getUser('nonexistent')
  
  expect(result).toBeNull()
  expect(mockRepository.findById).toHaveBeenCalledWith('nonexistent')
})


// ❌ Bad - mocking internal implementation details
vi.mock('./utils/hashPassword')  // Don't mock internal utils
```

### Mock Patterns
```typescript
// Simple mock
const mockFn = vi.fn()
mockFn.mockReturnValue('value')
mockFn.mockResolvedValue('async value')
mockFn.mockRejectedValue(new Error('failed'))

// Mock implementation
const mockFn = vi.fn().mockImplementation((id: string) => {
  if (id === 'special') return { id, special: true }
  return { id, special: false }
})

// Spy on existing method
const spy = vi.spyOn(userService, 'sendEmail')
spy.mockResolvedValue(undefined)

// Verify calls
expect(mockFn).toHaveBeenCalled()
expect(mockFn).toHaveBeenCalledTimes(2)
expect(mockFn).toHaveBeenCalledWith('arg1', 'arg2')
expect(mockFn).toHaveBeenLastCalledWith('final arg')
```

### Resetting Mocks
```typescript
describe('UserService', () => {
  const mockRepo = {
    findById: vi.fn(),
    create: vi.fn(),
  }
  
  beforeEach(() => {
    vi.clearAllMocks()  // Clear call history
    // or
    vi.resetAllMocks()  // Clear history + implementations
  })
  
  // Tests...
})
```

## Assertions

### Be Specific
```typescript
// ✅ Good - specific assertions
expect(user.email).toBe('test@example.com')
expect(users).toHaveLength(3)
expect(result).toEqual({ id: '123', name: 'Test' })
expect(error.message).toContain('not found')

// ❌ Bad - vague assertions
expect(user).toBeTruthy()
expect(result).toBeDefined()
expect(users.length > 0).toBe(true)
```

### Testing Objects
```typescript
// Exact match
expect(user).toEqual({
  id: '123',
  email: 'test@example.com',
  name: 'Test User',
})

// Partial match (when you only care about some fields)
expect(user).toMatchObject({
  email: 'test@example.com',
})

// Check structure, not values
expect(user).toEqual(expect.objectContaining({
  id: expect.any(String),
  createdAt: expect.any(Date),
}))
```

### Testing Arrays
```typescript
expect(users).toHaveLength(3)
expect(users).toContain(targetUser)
expect(users).toContainEqual({ id: '123', name: 'Test' })
expect(users).toEqual(expect.arrayContaining([user1, user2]))

// Order matters
expect(sortedUsers).toEqual([user1, user2, user3])

// Order doesn't matter
expect(users).toEqual(expect.arrayContaining([user3, user1, user2]))
```

### Testing Errors
```typescript
// Sync errors
expect(() => validateEmail('invalid')).toThrow()
expect(() => validateEmail('invalid')).toThrow(ValidationError)
expect(() => validateEmail('invalid')).toThrow('Invalid email format')

// Async errors
await expect(userService.getUser('bad-id')).rejects.toThrow()
await expect(userService.getUser('bad-id')).rejects.toThrow(NotFoundError)

// Check error properties
try {
  await userService.getUser('bad-id')
  fail('Expected error to be thrown')
} catch (error) {
  expect(error).toBeInstanceOf(NotFoundError)
  expect(error.code).toBe('NOT_FOUND')
  expect(error.statusCode).toBe(404)
}
```

## Testing Async Code

```typescript
// Using async/await (preferred)
it('should fetch user by id', async () => {
  mockRepo.findById.mockResolvedValue({ id: '123', name: 'Test' })
  
  const user = await userService.getUser('123')
  
  expect(user.name).toBe('Test')
})

// Testing Promise rejection
it('should throw when user not found', async () => {
  mockRepo.findById.mockResolvedValue(null)
  
  await expect(userService.getUser('nonexistent'))
    .rejects.toThrow(NotFoundError)
})

// Testing multiple async operations
it('should load dashboard data in parallel', async () => {
  mockUserService.getUser.mockResolvedValue(testUser)
  mockPostService.getPosts.mockResolvedValue(testPosts)
  
  const dashboard = await loadDashboard('user-123')
  
  expect(dashboard.user).toEqual(testUser)
  expect(dashboard.posts).toEqual(testPosts)
})
```

## Test Isolation

### Each Test is Independent
```typescript
// ✅ Good - isolated tests
describe('CartService', () => {
  let cartService: CartService
  let mockRepo: MockRepository
  
  beforeEach(() => {
    mockRepo = createMockRepository()
    cartService = new CartService(mockRepo)
  })
  
  it('should add item to cart', () => {
    cartService.addItem({ id: '1', price: 100 })
    expect(cartService.getTotal()).toBe(100)
  })
  
  it('should calculate total correctly', () => {
    // Starts fresh - previous test's item is not here
    cartService.addItem({ id: '1', price: 100 })
    cartService.addItem({ id: '2', price: 200 })
    expect(cartService.getTotal()).toBe(300)
  })
})


// ❌ Bad - shared state between tests
describe('CartService', () => {
  const cartService = new CartService()  // Shared instance!
  
  it('should add item', () => {
    cartService.addItem({ price: 100 })
    expect(cartService.getTotal()).toBe(100)
  })
  
  it('should calculate total', () => {
    // BUG: Has item from previous test!
    cartService.addItem({ price: 200 })
    expect(cartService.getTotal()).toBe(200)  // Fails! Gets 300
  })
})
```

## Edge Cases

### Always Test These
```typescript
describe('parseNumber', () => {
  // Happy path
  it('should parse valid integer', () => {
    expect(parseNumber('42')).toBe(42)
  })
  
  it('should parse valid float', () => {
    expect(parseNumber('3.14')).toBe(3.14)
  })
  
  // Edge cases - ALWAYS test these
  it('should handle empty string', () => {
    expect(parseNumber('')).toBeNaN()
  })
  
  it('should handle null', () => {
    expect(parseNumber(null)).toBeNaN()
  })
  
  it('should handle undefined', () => {
    expect(parseNumber(undefined)).toBeNaN()
  })
  
  it('should handle zero', () => {
    expect(parseNumber('0')).toBe(0)
  })
  
  it('should handle negative numbers', () => {
    expect(parseNumber('-5')).toBe(-5)
  })
  
  it('should handle very large numbers', () => {
    expect(parseNumber('999999999999')).toBe(999999999999)
  })
  
  it('should reject non-numeric strings', () => {
    expect(parseNumber('abc')).toBeNaN()
  })
})
```

### Boundary Conditions
```typescript
describe('pagination', () => {
  const items = range(1, 100)  // [1, 2, ..., 100]
  
  it('should return first page', () => {
    expect(paginate(items, { page: 1, size: 10 }))
      .toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  })
  
  it('should return last page', () => {
    expect(paginate(items, { page: 10, size: 10 }))
      .toEqual([91, 92, 93, 94, 95, 96, 97, 98, 99, 100])
  })
  
  it('should handle page beyond data', () => {
    expect(paginate(items, { page: 11, size: 10 }))
      .toEqual([])
  })
  
  it('should handle page size larger than data', () => {
    expect(paginate(items, { page: 1, size: 200 }))
      .toEqual(items)
  })
  
  it('should handle empty array', () => {
    expect(paginate([], { page: 1, size: 10 }))
      .toEqual([])
  })
})
```

## Code Coverage

### Meaningful Coverage
```typescript
// ✅ Good - tests behavior, achieves coverage as side effect
it('should apply discount for premium users', () => {
  const user = createTestUser({ isPremium: true })
  const result = calculateDiscount(100, user)
  expect(result).toBe(90)  // 10% premium discount
})

it('should not apply discount for regular users', () => {
  const user = createTestUser({ isPremium: false })
  const result = calculateDiscount(100, user)
  expect(result).toBe(100)  // No discount
})


// ❌ Bad - testing implementation to hit coverage
it('should call the isPremium getter', () => {
  const user = createTestUser()
  const spy = vi.spyOn(user, 'isPremium', 'get')
  calculateDiscount(100, user)
  expect(spy).toHaveBeenCalled()  // Who cares?
})
```

### Coverage Targets
- **Aim for 80%+ coverage** on business logic
- **Don't chase 100%** - diminishing returns
- **Focus on critical paths** - auth, payments, data transformations

## Anti-Patterns to Avoid

```typescript
// ❌ Testing implementation details
it('should call validateEmail before save', () => {
  const spy = vi.spyOn(utils, 'validateEmail')
  userService.create({ email: 'test@example.com' })
  expect(spy).toHaveBeenCalled()  // Brittle!
})

// ✅ Test the behavior instead
it('should reject invalid email', () => {
  await expect(userService.create({ email: 'invalid' }))
    .rejects.toThrow('Invalid email')
})


// ❌ Multiple assertions testing different behaviors
it('should handle user operations', () => {
  expect(userService.create(data)).resolves.toBeDefined()
  expect(userService.getUser('123')).resolves.toBeDefined()
  expect(userService.delete('123')).resolves.toBeUndefined()
})

// ✅ One test per behavior
it('should create user', () => { ... })
it('should get user by id', () => { ... })
it('should delete user', () => { ... })


// ❌ Brittle time-based tests
it('should set createdAt to now', () => {
  const user = userService.create(data)
  expect(user.createdAt).toEqual(new Date())  // Might fail!
})

// ✅ Use time mocking or ranges
it('should set createdAt to now', () => {
  vi.useFakeTimers()
  vi.setSystemTime(new Date('2024-01-15'))
  
  const user = userService.create(data)
  
  expect(user.createdAt).toEqual(new Date('2024-01-15'))
  vi.useRealTimers()
})
```

## Running Tests

```bash
# Run all tests
npm test

# Run specific file
npm test -- user.service.test.ts

# Run with coverage
npm test -- --coverage

# Watch mode (re-run on changes)
npm test -- --watch

# Run only tests matching pattern
npm test -- -t "should create user"
```

## Pre-Commit Checklist

- [ ] All new code has corresponding tests
- [ ] Tests follow AAA/Given-When-Then pattern
- [ ] Edge cases are covered (null, empty, boundary)
- [ ] Mocks are reset between tests
- [ ] No shared state between tests
- [ ] Tests are fast (< 100ms each)
- [ ] Test names describe behavior, not implementation
