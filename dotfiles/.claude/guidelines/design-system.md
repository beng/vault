# Design System Guidelines

> These guidelines define the visual standards agents must follow and verify.

## Design Tokens

### Colors

```typescript
// Use CSS variables or your design system's tokens
const colors = {
  // Primary
  primary: {
    50: '#eff6ff',
    100: '#dbeafe',
    500: '#3b82f6',   // Main brand color
    600: '#2563eb',   // Hover state
    700: '#1d4ed8',   // Active state
  },
  
  // Neutral
  neutral: {
    50: '#fafafa',
    100: '#f4f4f5',
    200: '#e4e4e7',
    300: '#d4d4d8',
    400: '#a1a1aa',
    500: '#71717a',
    600: '#52525b',
    700: '#3f3f46',
    800: '#27272a',
    900: '#18181b',
  },
  
  // Semantic
  success: '#22c55e',
  warning: '#f59e0b',
  error: '#ef4444',
  info: '#3b82f6',
}
```

### Typography

```typescript
const typography = {
  fontFamily: {
    sans: ['Inter', 'system-ui', 'sans-serif'],
    mono: ['JetBrains Mono', 'monospace'],
  },
  
  fontSize: {
    xs: '0.75rem',     // 12px
    sm: '0.875rem',    // 14px
    base: '1rem',      // 16px
    lg: '1.125rem',    // 18px
    xl: '1.25rem',     // 20px
    '2xl': '1.5rem',   // 24px
    '3xl': '1.875rem', // 30px
    '4xl': '2.25rem',  // 36px
  },
  
  fontWeight: {
    normal: 400,
    medium: 500,
    semibold: 600,
    bold: 700,
  },
  
  lineHeight: {
    tight: 1.25,
    normal: 1.5,
    relaxed: 1.75,
  },
}
```

### Spacing

```typescript
// 4px base unit
const spacing = {
  0: '0',
  1: '0.25rem',   // 4px
  2: '0.5rem',    // 8px
  3: '0.75rem',   // 12px
  4: '1rem',      // 16px
  5: '1.25rem',   // 20px
  6: '1.5rem',    // 24px
  8: '2rem',      // 32px
  10: '2.5rem',   // 40px
  12: '3rem',     // 48px
  16: '4rem',     // 64px
}
```

### Border Radius

```typescript
const borderRadius = {
  none: '0',
  sm: '0.125rem',   // 2px
  default: '0.25rem', // 4px
  md: '0.375rem',   // 6px
  lg: '0.5rem',     // 8px
  xl: '0.75rem',    // 12px
  full: '9999px',   // Pills/circles
}
```

### Shadows

```typescript
const shadows = {
  sm: '0 1px 2px 0 rgb(0 0 0 / 0.05)',
  default: '0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)',
  md: '0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)',
  lg: '0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1)',
}
```

---

## Component Standards

### Buttons

| Variant | Background | Text | Border | Hover | Active |
|---------|------------|------|--------|-------|--------|
| Primary | primary-500 | white | none | primary-600 | primary-700 |
| Secondary | white | neutral-700 | neutral-300 | neutral-50 | neutral-100 |
| Ghost | transparent | neutral-700 | none | neutral-100 | neutral-200 |
| Danger | error | white | none | darken 10% | darken 20% |

**Sizes:**
| Size | Height | Padding X | Font Size | Border Radius |
|------|--------|-----------|-----------|---------------|
| sm | 32px | 12px | 14px | md |
| md | 40px | 16px | 14px | md |
| lg | 48px | 24px | 16px | lg |

**States:**
- Hover: Cursor pointer, background shift
- Focus: Ring (2px, primary-500, offset 2px)
- Disabled: Opacity 50%, cursor not-allowed
- Loading: Spinner replaces text, disabled state

### Inputs

**Default state:**
- Border: neutral-300
- Background: white
- Text: neutral-900
- Placeholder: neutral-400

**States:**
- Focus: Border primary-500, ring (2px, primary-500/20)
- Error: Border error, ring (2px, error/20)
- Disabled: Background neutral-100, opacity 50%

**Sizes:** Match button sizes

### Cards

- Background: white
- Border: neutral-200 (1px)
- Border radius: lg
- Shadow: sm (hover: md)
- Padding: 16px (sm), 24px (md), 32px (lg)

### Modals

- Backdrop: neutral-900/50
- Container: white, rounded-xl, shadow-lg
- Max width: sm (384px), md (448px), lg (512px), xl (576px)
- Animation: Fade in + scale up (150ms ease-out)

---

## Interaction Standards

### Hover States

All interactive elements MUST have visible hover states:

```css
/* Buttons */
.btn:hover {
  background-color: var(--hover-bg);
  transform: translateY(-1px); /* subtle lift - optional */
}

/* Links */
a:hover {
  color: var(--primary-600);
  text-decoration: underline;
}

/* Cards */
.card:hover {
  box-shadow: var(--shadow-md);
  border-color: var(--neutral-300);
}

/* List items */
.list-item:hover {
  background-color: var(--neutral-50);
}
```

### Focus States

All focusable elements MUST have visible focus indicators:

```css
/* Standard focus ring */
:focus-visible {
  outline: 2px solid var(--primary-500);
  outline-offset: 2px;
}

/* For elements where outline doesn't work well */
.custom-focus:focus-visible {
  box-shadow: 0 0 0 2px var(--primary-500);
}
```

### Active States

```css
button:active {
  transform: translateY(1px);
}
```

### Disabled States

```css
:disabled,
[aria-disabled="true"] {
  opacity: 0.5;
  cursor: not-allowed;
  pointer-events: none;
}
```

### Transitions

Standard transition for interactive elements:

```css
.interactive {
  transition: all 150ms ease-in-out;
}

/* Or more specific */
.btn {
  transition: 
    background-color 150ms ease,
    transform 100ms ease,
    box-shadow 150ms ease;
}
```

---

## Layout Standards

### Container Widths

```css
.container {
  width: 100%;
  max-width: 1280px;
  margin: 0 auto;
  padding: 0 16px;
}

@media (min-width: 640px) {
  .container { padding: 0 24px; }
}

@media (min-width: 1024px) {
  .container { padding: 0 32px; }
}
```

### Breakpoints

```typescript
const breakpoints = {
  sm: '640px',   // Mobile landscape
  md: '768px',   // Tablet
  lg: '1024px',  // Desktop
  xl: '1280px',  // Large desktop
  '2xl': '1536px', // Extra large
}
```

### Z-Index Scale

```typescript
const zIndex = {
  dropdown: 10,
  sticky: 20,
  modal: 30,
  popover: 40,
  tooltip: 50,
  toast: 60,
}
```

---

## Accessibility Requirements

### Color Contrast

- Normal text: 4.5:1 minimum
- Large text (18px+ or 14px bold): 3:1 minimum
- UI components: 3:1 minimum

### Touch Targets

- Minimum size: 44x44px
- Spacing between targets: 8px minimum

### Motion

```css
@media (prefers-reduced-motion: reduce) {
  * {
    animation-duration: 0.01ms !important;
    transition-duration: 0.01ms !important;
  }
}
```

---

## Visual Regression Checkpoints

When verifying designs, check these specific states:

### For Every Component:
1. Default state
2. Hover state
3. Focus state (keyboard navigation)
4. Active/pressed state
5. Disabled state
6. Loading state (if applicable)
7. Error state (if applicable)

### For Every Page:
1. Desktop (1280px)
2. Tablet (768px)
3. Mobile (375px)
4. With long content (text overflow)
5. With missing content (empty states)

### Interactions to Verify:
1. Button clicks trigger expected action
2. Form inputs accept and display input
3. Dropdowns open/close correctly
4. Modals open/close with backdrop
5. Tooltips appear on hover
6. Navigation highlights current page
7. Scrolling behavior (sticky headers, infinite scroll)

---

## Common Issues Checklist

Before approving any UI:

- [ ] All buttons have hover/focus states
- [ ] All inputs have focus states
- [ ] All interactive elements are keyboard accessible
- [ ] Colors meet contrast requirements
- [ ] Touch targets are large enough
- [ ] Loading states exist for async operations
- [ ] Error states are visually distinct
- [ ] Empty states are designed (not blank)
- [ ] Responsive design works at all breakpoints
- [ ] Animations respect reduced-motion preference
