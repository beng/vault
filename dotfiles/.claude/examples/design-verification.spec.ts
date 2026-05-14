/**
 * Design Verification Tests
 * 
 * Run these tests to verify UI matches design system standards.
 * 
 * Usage:
 *   npx playwright test tests/e2e/design-verification.spec.ts
 *   npx playwright test tests/e2e/design-verification.spec.ts --update-snapshots
 */

import { test, expect, Page } from '@playwright/test'
import AxeBuilder from '@axe-core/playwright'

// Design tokens - update these to match your design-system.md
const DESIGN_TOKENS = {
  colors: {
    primary500: 'rgb(59, 130, 246)',
    primary600: 'rgb(37, 99, 235)',
    error: 'rgb(239, 68, 68)',
    neutral50: 'rgb(250, 250, 250)',
    neutral700: 'rgb(63, 63, 70)',
  },
  spacing: {
    4: '16px',
    6: '24px',
  },
  borderRadius: {
    md: '6px',
    lg: '8px',
  },
  fontSize: {
    sm: '14px',
    base: '16px',
  },
}

const VIEWPORTS = {
  mobile: { width: 375, height: 667 },
  tablet: { width: 768, height: 1024 },
  desktop: { width: 1280, height: 800 },
}

// Helper to get computed style
async function getStyle(page: Page, selector: string, property: string): Promise<string> {
  return page.locator(selector).evaluate(
    (el, prop) => getComputedStyle(el).getPropertyValue(prop),
    property
  )
}

// =============================================================================
// Component State Tests
// =============================================================================

test.describe('Button Component', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to component or page with buttons
    await page.goto('/components/button') // Adjust URL
  })

  test('primary button - default state', async ({ page }) => {
    const button = page.getByRole('button', { name: 'Primary Button' })
    
    // Visual snapshot
    await expect(button).toHaveScreenshot('button-primary-default.png')
    
    // Verify design tokens
    const bgColor = await button.evaluate(el => getComputedStyle(el).backgroundColor)
    expect(bgColor).toBe(DESIGN_TOKENS.colors.primary500)
  })

  test('primary button - hover state', async ({ page }) => {
    const button = page.getByRole('button', { name: 'Primary Button' })
    
    await button.hover()
    await expect(button).toHaveScreenshot('button-primary-hover.png')
    
    const bgColor = await button.evaluate(el => getComputedStyle(el).backgroundColor)
    expect(bgColor).toBe(DESIGN_TOKENS.colors.primary600)
  })

  test('primary button - focus state', async ({ page }) => {
    const button = page.getByRole('button', { name: 'Primary Button' })
    
    await button.focus()
    await expect(button).toHaveScreenshot('button-primary-focus.png')
    
    // Check focus ring exists
    const outline = await button.evaluate(el => getComputedStyle(el).outline)
    expect(outline).not.toBe('none')
  })

  test('primary button - disabled state', async ({ page }) => {
    const button = page.getByRole('button', { name: 'Disabled Button' })
    
    await expect(button).toBeDisabled()
    await expect(button).toHaveScreenshot('button-primary-disabled.png')
    
    const opacity = await button.evaluate(el => getComputedStyle(el).opacity)
    expect(parseFloat(opacity)).toBeLessThanOrEqual(0.5)
  })

  test('button sizes match design system', async ({ page }) => {
    const sizes = [
      { name: 'Small Button', expectedHeight: '32px' },
      { name: 'Medium Button', expectedHeight: '40px' },
      { name: 'Large Button', expectedHeight: '48px' },
    ]

    for (const size of sizes) {
      const button = page.getByRole('button', { name: size.name })
      const box = await button.boundingBox()
      expect(box?.height).toBe(parseInt(size.expectedHeight))
    }
  })
})

// =============================================================================
// Form Input Tests
// =============================================================================

test.describe('Form Inputs', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/components/input') // Adjust URL
  })

  test('text input - focus state has visible ring', async ({ page }) => {
    const input = page.getByLabel('Email')
    
    await input.focus()
    await expect(input).toHaveScreenshot('input-text-focus.png')
    
    // Check for focus ring (outline or box-shadow)
    const outline = await input.evaluate(el => getComputedStyle(el).outline)
    const boxShadow = await input.evaluate(el => getComputedStyle(el).boxShadow)
    
    const hasFocusIndicator = outline !== 'none' || boxShadow !== 'none'
    expect(hasFocusIndicator).toBe(true)
  })

  test('text input - error state', async ({ page }) => {
    const input = page.getByLabel('Email')
    
    // Trigger error state (adjust based on your implementation)
    await input.fill('invalid')
    await input.blur()
    
    await expect(input).toHaveScreenshot('input-text-error.png')
    
    const borderColor = await input.evaluate(el => getComputedStyle(el).borderColor)
    expect(borderColor).toBe(DESIGN_TOKENS.colors.error)
  })
})

// =============================================================================
// Interaction Tests
// =============================================================================

test.describe('Interactions', () => {
  test('dropdown opens and closes correctly', async ({ page }) => {
    await page.goto('/components/dropdown')
    
    const trigger = page.getByRole('button', { name: 'Open Menu' })
    const menu = page.getByRole('menu')
    
    // Initially closed
    await expect(menu).not.toBeVisible()
    
    // Click to open
    await trigger.click()
    await expect(menu).toBeVisible()
    await expect(menu).toHaveScreenshot('dropdown-open.png')
    
    // Click outside to close
    await page.click('body', { position: { x: 0, y: 0 } })
    await expect(menu).not.toBeVisible()
    
    // Escape key also closes
    await trigger.click()
    await expect(menu).toBeVisible()
    await page.keyboard.press('Escape')
    await expect(menu).not.toBeVisible()
  })

  test('modal opens with animation and closes on backdrop click', async ({ page }) => {
    await page.goto('/components/modal')
    
    const openButton = page.getByRole('button', { name: 'Open Modal' })
    const modal = page.getByRole('dialog')
    const backdrop = page.locator('[data-testid="modal-backdrop"]')
    
    // Open modal
    await openButton.click()
    await expect(modal).toBeVisible()
    await expect(modal).toHaveScreenshot('modal-open.png')
    
    // Close via backdrop
    await backdrop.click({ position: { x: 10, y: 10 } })
    await expect(modal).not.toBeVisible()
  })

  test('tooltip appears on hover with correct delay', async ({ page }) => {
    await page.goto('/components/tooltip')
    
    const trigger = page.getByTestId('tooltip-trigger')
    const tooltip = page.getByRole('tooltip')
    
    // Not visible initially
    await expect(tooltip).not.toBeVisible()
    
    // Hover and wait for tooltip
    await trigger.hover()
    
    // Should appear within 200ms
    await expect(tooltip).toBeVisible({ timeout: 200 })
    await expect(tooltip).toHaveScreenshot('tooltip-visible.png')
    
    // Should disappear when not hovering
    await page.mouse.move(0, 0)
    await expect(tooltip).not.toBeVisible()
  })
})

// =============================================================================
// Responsive Layout Tests
// =============================================================================

test.describe('Responsive Layouts', () => {
  const pages = ['/', '/dashboard', '/settings'] // Adjust to your pages

  for (const pagePath of pages) {
    for (const [name, viewport] of Object.entries(VIEWPORTS)) {
      test(`${pagePath} - ${name} viewport`, async ({ page }) => {
        await page.setViewportSize(viewport)
        await page.goto(pagePath)
        
        // Wait for any lazy-loaded content
        await page.waitForLoadState('networkidle')
        
        // Full page screenshot
        await expect(page).toHaveScreenshot(`${pagePath.replace('/', '-')}-${name}.png`, {
          fullPage: true,
        })
        
        // Check no horizontal overflow
        const hasHorizontalScroll = await page.evaluate(() => 
          document.documentElement.scrollWidth > document.documentElement.clientWidth
        )
        expect(hasHorizontalScroll).toBe(false)
      })
    }
  }
})

// =============================================================================
// Accessibility Tests
// =============================================================================

test.describe('Accessibility', () => {
  const pages = ['/', '/dashboard', '/settings'] // Adjust to your pages

  for (const pagePath of pages) {
    test(`${pagePath} - no accessibility violations`, async ({ page }) => {
      await page.goto(pagePath)
      await page.waitForLoadState('networkidle')
      
      const results = await new AxeBuilder({ page })
        .withTags(['wcag2a', 'wcag2aa'])
        .analyze()
      
      // Log violations for debugging
      if (results.violations.length > 0) {
        console.log('Accessibility violations:', JSON.stringify(results.violations, null, 2))
      }
      
      expect(results.violations).toHaveLength(0)
    })
  }

  test('keyboard navigation works correctly', async ({ page }) => {
    await page.goto('/')
    
    // Tab through interactive elements
    const focusedElements: string[] = []
    
    for (let i = 0; i < 10; i++) {
      await page.keyboard.press('Tab')
      const focused = await page.evaluate(() => {
        const el = document.activeElement
        return el?.tagName + (el?.getAttribute('data-testid') || '')
      })
      focusedElements.push(focused)
    }
    
    // Verify focus moved (not stuck on body)
    const uniqueElements = new Set(focusedElements)
    expect(uniqueElements.size).toBeGreaterThan(1)
  })

  test('touch targets are large enough', async ({ page }) => {
    await page.goto('/')
    
    const interactiveElements = page.locator('button, a, input, [role="button"]')
    const count = await interactiveElements.count()
    
    for (let i = 0; i < count; i++) {
      const element = interactiveElements.nth(i)
      const box = await element.boundingBox()
      
      if (box) {
        // Minimum 44x44 for touch targets
        expect(box.width, `Element ${i} width`).toBeGreaterThanOrEqual(44)
        expect(box.height, `Element ${i} height`).toBeGreaterThanOrEqual(44)
      }
    }
  })
})

// =============================================================================
// Color Contrast Tests
// =============================================================================

test.describe('Color Contrast', () => {
  test('text elements meet WCAG AA contrast', async ({ page }) => {
    await page.goto('/')
    
    const results = await new AxeBuilder({ page })
      .withRules(['color-contrast'])
      .analyze()
    
    expect(results.violations).toHaveLength(0)
  })
})
