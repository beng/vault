# Design Spec Section

> Add this section to your spec when the feature includes UI components.

---

## Design Requirements

### Visual Design

**Color Usage:**
- Primary actions: `primary-500` buttons
- Secondary actions: `neutral` outlined buttons
- Destructive actions: `error` colored buttons
- Status indicators: `success`, `warning`, `error`, `info`

**Typography:**
- Page title: `text-2xl font-semibold`
- Section headers: `text-lg font-medium`
- Body text: `text-base`
- Helper text: `text-sm text-neutral-500`

**Spacing:**
- Section padding: `p-6`
- Card padding: `p-4`
- Element gaps: `gap-4` (default), `gap-2` (compact)

### Component Specifications

#### [Component Name]

**Variants:**
| Variant | When to Use |
|---------|-------------|
| Default | Standard usage |
| Compact | In tables, dense lists |
| Prominent | Call-to-action areas |

**States:**
- Default: [description]
- Hover: [description]
- Active: [description]
- Disabled: [description]
- Loading: [description]
- Error: [description]

**Responsive Behavior:**
- Desktop (1024px+): [layout description]
- Tablet (768px-1023px): [layout description]
- Mobile (<768px): [layout description]

### Interaction Specifications

| Trigger | Action | Feedback |
|---------|--------|----------|
| Button click | Submit form | Loading spinner, then success toast |
| Item hover | Show preview | Tooltip after 150ms |
| Escape key | Close modal | Fade out animation |
| Outside click | Close dropdown | Immediate close |

### Animation & Transitions

| Element | Property | Duration | Easing |
|---------|----------|----------|--------|
| Button hover | background-color | 150ms | ease |
| Modal open | opacity, transform | 200ms | ease-out |
| Toast enter | transform | 300ms | spring |
| Dropdown | opacity, transform | 150ms | ease |

### Accessibility Requirements

- [ ] All interactive elements focusable via keyboard
- [ ] Focus order follows visual order
- [ ] Focus indicators visible (2px ring)
- [ ] Color contrast meets WCAG AA (4.5:1 text, 3:1 UI)
- [ ] Touch targets minimum 44x44px
- [ ] Motion respects `prefers-reduced-motion`
- [ ] Screen reader labels on icon-only buttons
- [ ] Form errors announced to screen readers

### Design Verification Checklist

After implementation, verify:

**Visual:**
- [ ] Colors match design tokens exactly
- [ ] Typography matches scale
- [ ] Spacing is consistent with system
- [ ] Icons are correct size and color
- [ ] Images have proper aspect ratios

**States:**
- [ ] All hover states implemented
- [ ] All focus states visible
- [ ] All disabled states styled
- [ ] All loading states implemented
- [ ] All error states styled

**Responsive:**
- [ ] Layout works at 1280px (desktop)
- [ ] Layout works at 768px (tablet)
- [ ] Layout works at 375px (mobile)
- [ ] No horizontal scroll at any breakpoint
- [ ] Touch targets adequate on mobile

**Interactions:**
- [ ] All clicks trigger expected actions
- [ ] All keyboard shortcuts work
- [ ] All animations smooth (60fps)
- [ ] No layout shift during interactions

---

## Example: Notification Bell Component

### Visual Design

```
┌─────────────────────┐
│  🔔  (badge: 3)     │  ← Icon button, 40x40px
└─────────────────────┘
         │
         ▼ click
┌─────────────────────────────────────┐
│ Notifications            Mark all  │  ← Header with action
├─────────────────────────────────────┤
│ ● New comment on your post         │  ← Unread (blue dot)
│   2 minutes ago                    │
├─────────────────────────────────────┤
│   Your post was liked              │  ← Read (no dot)
│   1 hour ago                       │
├─────────────────────────────────────┤
│         Load more                  │  ← Pagination
└─────────────────────────────────────┘
```

### States

| State | Visual |
|-------|--------|
| No notifications | Badge hidden, empty state in dropdown |
| Has unread | Red badge with count (max "9+") |
| Dropdown open | Bell highlighted, dropdown visible |
| Loading more | Spinner at bottom of list |

### Interactions

| Trigger | Result |
|---------|--------|
| Click bell | Toggle dropdown |
| Click notification | Mark read + navigate to link |
| Click "Mark all" | All notifications marked read |
| Click outside | Close dropdown |
| Escape key | Close dropdown |

### Responsive

- **Desktop**: Dropdown positioned below-right of bell
- **Mobile**: Full-screen modal instead of dropdown
