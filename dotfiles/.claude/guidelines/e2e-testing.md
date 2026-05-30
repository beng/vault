# E2E & UI Testing Guidelines

> These guidelines are mandatory for all end-to-end and UI tests.

## Overview

E2E tests verify the application works correctly from a user's perspective. We use **Playwright** for headless browser testing because:
- Cross-browser support (Chromium, Firefox, WebKit)
- Auto-wait (no manual sleep/wait)
- Powerful selectors and assertions
- Built-in test runner with parallelization
- Trace viewer for debugging

## Setup

### Installation
```bash
# Install Playwright
npm install -D @playwright/test

# Install browsers
npx playwright install
```

### Configuration
```typescript
// playwright.config.ts
import { defineConfig, devices } from '@playwright/test'

export default defineConfig({
  testDir: './tests/e2e',
  timeout: 30000,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  
  use: {
    baseURL: process.env.BASE_URL || 'http://localhost:3000',
    trace: 'on-first-retry',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
    {
      name: 'firefox',
      use: { ...devices['Desktop Firefox'] },
    },
    {
      name: 'webkit',
      use: { ...devices['Desktop Safari'] },
    },
    {
      name: 'mobile',
      use: { ...devices['iPhone 13'] },
    },
  ],
  
  webServer: {
    command: 'npm run dev',
    url: 'http://localhost:3000',
    reuseExistingServer: !process.env.CI,
  },
})
```

## Project Structure

```
tests/
├── e2e/
│   ├── auth/
│   │   ├── login.spec.ts
│   │   ├── logout.spec.ts
│   │   └── register.spec.ts
│   ├── dashboard/
│   │   └── dashboard.spec.ts
│   ├── settings/
│   │   └── profile.spec.ts
│   └── flows/
│       └── purchase.spec.ts
├── fixtures/
│   ├── auth.fixture.ts
│   ├── test-users.ts
│   └── test-data.ts
├── pages/                      # Page Object Models
│   ├── login.page.ts
│   ├── dashboard.page.ts
│   └── base.page.ts
└── utils/
    ├── api-helpers.ts
    └── test-helpers.ts
```

## Writing Tests

### Basic Test Structure
```typescript
import { test, expect } from '@playwright/test'

test.describe('Login', () => {
  test('should login with valid credentials', async ({ page }) => {
    // Navigate
    await page.goto('/login')
    
    // Fill form
    await page.getByLabel('Email').fill('test@example.com')
    await page.getByLabel('Password').fill('password123')
    
    // Submit
    await page.getByRole('button', { name: 'Sign in' }).click()
    
    // Assert
    await expect(page).toHaveURL('/dashboard')
    await expect(page.getByText('Welcome back')).toBeVisible()
  })
  
  test('should show error for invalid credentials', async ({ page }) => {
    await page.goto('/login')
    
    await page.getByLabel('Email').fill('wrong@example.com')
    await page.getByLabel('Password').fill('wrongpassword')
    await page.getByRole('button', { name: 'Sign in' }).click()
    
    await expect(page.getByText('Invalid email or password')).toBeVisible()
    await expect(page).toHaveURL('/login')  // Still on login page
  })
})
```

## Selectors

### Priority Order (Most Reliable → Least)
```typescript
// 1. Role-based (BEST - accessible and stable)
await page.getByRole('button', { name: 'Submit' })
await page.getByRole('link', { name: 'Home' })
await page.getByRole('textbox', { name: 'Email' })
await page.getByRole('checkbox', { name: 'Remember me' })

// 2. Label-based (form elements)
await page.getByLabel('Email address')
await page.getByLabel('Password')

// 3. Placeholder (when no label)
await page.getByPlaceholder('Enter your email')

// 4. Text content (visible text)
await page.getByText('Welcome back')
await page.getByText('Sign in', { exact: true })  // Exact match

// 5. Test ID (when nothing else works)
await page.getByTestId('submit-button')
await page.getByTestId('user-avatar')

// 6. CSS/XPath (LAST RESORT - brittle)
await page.locator('.submit-btn')  // Avoid
await page.locator('#login-form')  // Avoid
```

### Adding Test IDs
```tsx
// When semantic selectors won't work, add test IDs
<button data-testid="submit-payment">Pay Now</button>
<div data-testid="user-menu">...</div>
```

### Filtering and Chaining
```typescript
// Filter by multiple attributes
await page.getByRole('listitem')
  .filter({ hasText: 'Product A' })
  .getByRole('button', { name: 'Add to cart' })
  .click()

// Find within container
const modal = page.getByRole('dialog')
await modal.getByRole('button', { name: 'Confirm' }).click()

// nth element
await page.getByRole('listitem').nth(2).click()
await page.getByRole('listitem').first().click()
await page.getByRole('listitem').last().click()
```

## Page Object Model

### Base Page
```typescript
// pages/base.page.ts
import { Page, Locator } from '@playwright/test'

export abstract class BasePage {
  constructor(protected page: Page) {}
  
  async navigate(): Promise<void> {
    await this.page.goto(this.path)
  }
  
  protected abstract get path(): string
  
  // Common elements
  get header(): Locator {
    return this.page.getByRole('banner')
  }
  
  get footer(): Locator {
    return this.page.getByRole('contentinfo')
  }
  
  async getToastMessage(): Promise<string> {
    const toast = this.page.getByRole('alert')
    await toast.waitFor()
    return toast.textContent() ?? ''
  }
}
```

### Page Implementation
```typescript
// pages/login.page.ts
import { Page, Locator, expect } from '@playwright/test'
import { BasePage } from './base.page'

export class LoginPage extends BasePage {
  protected get path(): string {
    return '/login'
  }
  
  // Locators
  get emailInput(): Locator {
    return this.page.getByLabel('Email')
  }
  
  get passwordInput(): Locator {
    return this.page.getByLabel('Password')
  }
  
  get submitButton(): Locator {
    return this.page.getByRole('button', { name: 'Sign in' })
  }
  
  get errorMessage(): Locator {
    return this.page.getByRole('alert')
  }
  
  // Actions
  async login(email: string, password: string): Promise<void> {
    await this.emailInput.fill(email)
    await this.passwordInput.fill(password)
    await this.submitButton.click()
  }
  
  // Assertions (page-specific)
  async expectErrorMessage(message: string): Promise<void> {
    await expect(this.errorMessage).toContainText(message)
  }
  
  async expectToBeOnLoginPage(): Promise<void> {
    await expect(this.page).toHaveURL(/\/login/)
  }
}
```

### Using Page Objects in Tests
```typescript
// tests/e2e/auth/login.spec.ts
import { test, expect } from '@playwright/test'
import { LoginPage } from '../../pages/login.page'
import { DashboardPage } from '../../pages/dashboard.page'

test.describe('Login', () => {
  let loginPage: LoginPage
  
  test.beforeEach(async ({ page }) => {
    loginPage = new LoginPage(page)
    await loginPage.navigate()
  })
  
  test('should login successfully', async ({ page }) => {
    await loginPage.login('test@example.com', 'password123')
    
    const dashboard = new DashboardPage(page)
    await dashboard.expectToBeOnDashboard()
    await expect(dashboard.welcomeMessage).toContainText('Welcome')
  })
  
  test('should show error for invalid credentials', async () => {
    await loginPage.login('wrong@example.com', 'wrongpassword')
    
    await loginPage.expectErrorMessage('Invalid email or password')
    await loginPage.expectToBeOnLoginPage()
  })
})
```

## Fixtures

### Authentication Fixture
```typescript
// fixtures/auth.fixture.ts
import { test as base, Page } from '@playwright/test'

type AuthFixtures = {
  authenticatedPage: Page
  adminPage: Page
}

export const test = base.extend<AuthFixtures>({
  authenticatedPage: async ({ page }, use) => {
    // Login via API (faster than UI)
    await page.request.post('/api/auth/login', {
      data: { email: 'test@example.com', password: 'password123' }
    })
    
    // Or set auth cookies directly
    await page.context().addCookies([{
      name: 'auth_token',
      value: 'test-token',
      domain: 'localhost',
      path: '/',
    }])
    
    await use(page)
  },
  
  adminPage: async ({ page }, use) => {
    await page.request.post('/api/auth/login', {
      data: { email: 'admin@example.com', password: 'admin123' }
    })
    await use(page)
  },
})

export { expect } from '@playwright/test'
```

### Using Fixtures
```typescript
// tests/e2e/dashboard/dashboard.spec.ts
import { test, expect } from '../../fixtures/auth.fixture'

test.describe('Dashboard', () => {
  test('should show user stats', async ({ authenticatedPage }) => {
    await authenticatedPage.goto('/dashboard')
    
    await expect(authenticatedPage.getByTestId('stats-panel')).toBeVisible()
    await expect(authenticatedPage.getByText('Total Orders')).toBeVisible()
  })
  
  test('admin should see admin panel', async ({ adminPage }) => {
    await adminPage.goto('/dashboard')
    
    await expect(adminPage.getByRole('link', { name: 'Admin Panel' })).toBeVisible()
  })
})
```

## Waiting and Assertions

### Auto-Wait (Playwright handles most waiting)
```typescript
// Playwright auto-waits for these:
await page.click('button')           // Waits for button to be clickable
await page.fill('input', 'text')     // Waits for input to be editable
await expect(locator).toBeVisible()  // Waits for visibility

// You rarely need explicit waits
```

### When You Do Need to Wait
```typescript
// Wait for navigation
await page.waitForURL('/dashboard')
await page.waitForURL(/\/users\/\d+/)

// Wait for network idle (all requests complete)
await page.waitForLoadState('networkidle')

// Wait for specific request
const responsePromise = page.waitForResponse('/api/users')
await page.click('button')
const response = await responsePromise

// Wait for element state
await page.getByTestId('loader').waitFor({ state: 'hidden' })
await page.getByRole('dialog').waitFor({ state: 'visible' })
```

### Assertions
```typescript
// Element visibility
await expect(page.getByText('Success')).toBeVisible()
await expect(page.getByText('Loading')).toBeHidden()
await expect(page.getByTestId('modal')).not.toBeVisible()

// Element state
await expect(page.getByRole('button')).toBeEnabled()
await expect(page.getByRole('button')).toBeDisabled()
await expect(page.getByRole('checkbox')).toBeChecked()

// Text content
await expect(page.getByTestId('message')).toHaveText('Success!')
await expect(page.getByTestId('message')).toContainText('Success')

// Attributes
await expect(page.getByRole('link')).toHaveAttribute('href', '/home')
await expect(page.getByRole('input')).toHaveValue('test@example.com')

// Page state
await expect(page).toHaveURL('/dashboard')
await expect(page).toHaveTitle('Dashboard | App')

// Count
await expect(page.getByRole('listitem')).toHaveCount(5)

// Screenshot comparison (visual regression)
await expect(page).toHaveScreenshot('dashboard.png')
await expect(page.getByTestId('chart')).toHaveScreenshot('chart.png')
```

## Testing Forms

```typescript
test('should submit contact form', async ({ page }) => {
  await page.goto('/contact')
  
  // Fill text inputs
  await page.getByLabel('Name').fill('John Doe')
  await page.getByLabel('Email').fill('john@example.com')
  await page.getByLabel('Message').fill('Hello, this is my message.')
  
  // Select dropdown
  await page.getByLabel('Subject').selectOption('support')
  // or by value
  await page.getByLabel('Subject').selectOption({ value: 'support' })
  // or by label
  await page.getByLabel('Subject').selectOption({ label: 'Technical Support' })
  
  // Checkbox
  await page.getByLabel('Subscribe to newsletter').check()
  
  // Radio button
  await page.getByLabel('Email').check()  // Radio option
  
  // File upload
  await page.getByLabel('Attachment').setInputFiles('path/to/file.pdf')
  // Multiple files
  await page.getByLabel('Attachments').setInputFiles(['file1.pdf', 'file2.pdf'])
  
  // Submit
  await page.getByRole('button', { name: 'Send Message' }).click()
  
  // Assert success
  await expect(page.getByText('Message sent successfully')).toBeVisible()
})
```

## Testing Modals and Dialogs

```typescript
test('should confirm deletion', async ({ page }) => {
  await page.goto('/users')
  
  // Click delete button
  await page.getByRole('row', { name: /John Doe/ })
    .getByRole('button', { name: 'Delete' })
    .click()
  
  // Wait for and interact with modal
  const modal = page.getByRole('dialog')
  await expect(modal).toBeVisible()
  await expect(modal.getByText('Are you sure?')).toBeVisible()
  
  // Confirm deletion
  await modal.getByRole('button', { name: 'Delete' }).click()
  
  // Modal should close
  await expect(modal).not.toBeVisible()
  
  // User should be removed
  await expect(page.getByRole('row', { name: /John Doe/ })).not.toBeVisible()
})

// Native browser dialogs
test('should handle browser confirm', async ({ page }) => {
  // Set up dialog handler BEFORE triggering
  page.on('dialog', async dialog => {
    expect(dialog.message()).toBe('Are you sure?')
    await dialog.accept()  // or dialog.dismiss()
  })
  
  await page.getByRole('button', { name: 'Delete All' }).click()
})
```

## Testing Navigation

```typescript
test('should navigate through app', async ({ page }) => {
  await page.goto('/')
  
  // Click navigation link
  await page.getByRole('link', { name: 'Products' }).click()
  await expect(page).toHaveURL('/products')
  
  // Click product card
  await page.getByRole('article').first().click()
  await expect(page).toHaveURL(/\/products\/\d+/)
  
  // Go back
  await page.goBack()
  await expect(page).toHaveURL('/products')
  
  // Use keyboard
  await page.keyboard.press('Alt+ArrowLeft')  // Browser back
})
```

## Testing API Interactions

```typescript
test('should load and display users', async ({ page }) => {
  // Intercept API call
  await page.route('/api/users', async route => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: JSON.stringify([
        { id: 1, name: 'Alice' },
        { id: 2, name: 'Bob' },
      ]),
    })
  })
  
  await page.goto('/users')
  
  await expect(page.getByRole('listitem')).toHaveCount(2)
  await expect(page.getByText('Alice')).toBeVisible()
  await expect(page.getByText('Bob')).toBeVisible()
})

test('should handle API errors gracefully', async ({ page }) => {
  await page.route('/api/users', route => 
    route.fulfill({ status: 500, body: 'Server error' })
  )
  
  await page.goto('/users')
  
  await expect(page.getByText('Failed to load users')).toBeVisible()
  await expect(page.getByRole('button', { name: 'Retry' })).toBeVisible()
})
```

## Running Tests

```bash
# Run all tests
npx playwright test

# Run specific file
npx playwright test login.spec.ts

# Run tests with specific tag
npx playwright test --grep @smoke

# Run in headed mode (see browser)
npx playwright test --headed

# Run in specific browser
npx playwright test --project=chromium
npx playwright test --project=firefox

# Run with UI mode (interactive)
npx playwright test --ui

# Debug mode
npx playwright test --debug

# Generate report
npx playwright show-report
```

## CI Configuration

```yaml
# .github/workflows/e2e.yml
name: E2E Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          
      - name: Install dependencies
        run: npm ci
        
      - name: Install Playwright browsers
        run: npx playwright install --with-deps
        
      - name: Run E2E tests
        run: npx playwright test
        env:
          BASE_URL: http://localhost:3000
          
      - uses: actions/upload-artifact@v4
        if: failure()
        with:
          name: playwright-report
          path: playwright-report/
          retention-days: 7
```

## Anti-Patterns to Avoid

```typescript
// ❌ Hard-coded waits
await page.waitForTimeout(3000)  // Never do this!

// ✅ Wait for specific condition
await expect(page.getByText('Loaded')).toBeVisible()


// ❌ Brittle CSS selectors
await page.click('.btn.btn-primary.submit-form')
await page.click('#root > div > form > button')

// ✅ Semantic selectors
await page.getByRole('button', { name: 'Submit' }).click()


// ❌ Not waiting for page state
await page.goto('/dashboard')
const text = await page.textContent('.stats')  // Might be null!

// ✅ Use assertions that auto-wait
await page.goto('/dashboard')
await expect(page.getByTestId('stats')).toHaveText('100')


// ❌ Tests that depend on each other
test('should create user', async ({ page }) => { ... })
test('should edit user', async ({ page }) => {
  // Assumes previous test ran!
})

// ✅ Independent tests with proper setup
test('should edit user', async ({ page }) => {
  // Create user via API first
  const user = await createTestUser()
  await page.goto(`/users/${user.id}/edit`)
  // ...
})
```

## Pre-Commit Checklist

- [ ] Tests use semantic selectors (roles, labels, text)
- [ ] No hard-coded timeouts
- [ ] Tests are independent (can run in any order)
- [ ] Page Objects used for reusable interactions
- [ ] API mocking for unreliable external services
- [ ] Tests pass in CI (headless mode)
- [ ] Screenshots/traces configured for failures
