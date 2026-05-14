# React Component Guidelines

These guidelines are mandatory for React code. They extend the TypeScript guidelines with React-specific patterns.

## Core Principles

1. **Single Responsibility** - One component = one job
2. **Composition Over Configuration** - Use children, not props
3. **Functional Components Only** - No class components
4. **Hooks for Logic** - Extract reusable logic into custom hooks
5. **Colocate State** - State lives as close to usage as possible

---

## Component Patterns

### 15. Single Responsibility

One component = one job. If you're adding "and" to describe it, split it.

```typescript
// ✗ BAD - does too much
function UserListWithSearchAndPagination() { ... }

// ✓ GOOD - compose single-purpose components
function UserPage() {
  return (
    <SearchProvider>
      <SearchInput />
      <UserList />
      <Pagination />
    </SearchProvider>
  );
}
```

### 16. Composition Over Configuration

Prefer children and composition over prop drilling.

```typescript
// ✗ BAD - configuration via props
<Modal
  title="Confirm"
  body={<p>Are you sure?</p>}
  footer={<Button>OK</Button>}
/>

// ✓ GOOD - composition via children
<Modal>
  <ModalHeader>Confirm</ModalHeader>
  <ModalBody>
    <p>Are you sure?</p>
  </ModalBody>
  <ModalFooter>
    <Button>OK</Button>
  </ModalFooter>
</Modal>
```

### 17. Shell Components Stay Thin

Container/shell components compose children. They don't define domain models.

```typescript
// ✓ GOOD - shell receives data, composes children
function Drawer({ children, isOpen, onClose }: DrawerProps) {
  return (
    <div className={isOpen ? 'open' : 'closed'}>
      <button onClick={onClose}>×</button>
      {children}
    </div>
  );
}

// Shell doesn't know what Item is - it just renders
function ListPanel<T>({ items, renderItem }: ListPanelProps<T>) {
  return (
    <Drawer>
      {items.map(renderItem)}
    </Drawer>
  );
}
```

### 18. Props Stay Minimal

Only pass what the component needs to render.

```typescript
// ✗ BAD - passing entire object when only name needed
<Header user={user} />
function Header({ user }: { user: User }) {
  return <h1>Hello, {user.name}</h1>;
}

// ✓ GOOD - pass only what's needed
<Header userName={user.name} />
function Header({ userName }: { userName: string }) {
  return <h1>Hello, {userName}</h1>;
}
```

### 19. Props Interface Convention

```typescript
// ✓ ComponentNameProps
interface ButtonProps {
  variant: 'primary' | 'secondary';
  onClick: () => void;
  children: React.ReactNode;
}

function Button({ variant, onClick, children }: ButtonProps) { ... }
```

### 20. Event Handler Naming

```typescript
// Props: onEventName (passed from parent)
// Internal: handleEventName (defined in component)

interface ListProps {
  onItemSelect: (id: string) => void; // Prop
}

function List({ onItemSelect }: ListProps) {
  const handleClick = (id: string) => { // Internal handler
    console.log('[List] Item clicked', { id });
    onItemSelect(id);
  };

  return <Item onClick={() => handleClick(item.id)} />;
}
```

### 21. Boolean Props

Use is/has/should/can prefixes:

```typescript
// ✓ GOOD
interface PanelProps {
  isExpanded: boolean;
  isLoading: boolean;
  hasError: boolean;
  canEdit: boolean;
  shouldAnimate: boolean;
}

// ✗ BAD - ambiguous
interface PanelProps {
  expanded: boolean;
  loading: boolean;
  error: boolean;
}
```

---

## State Management

### 22. Colocate State

State lives as close to where it's used as possible.

```typescript
// ✗ BAD - state too high, only DetailPanel uses viewMode
function App() {
  const [viewMode, setViewMode] = useState('list');
  return <DetailPanel viewMode={viewMode} onViewModeChange={setViewMode} />;
}

// ✓ GOOD - state colocated where it's used
function App() {
  return <DetailPanel />;
}

function DetailPanel() {
  const [viewMode, setViewMode] = useState('list'); // Owns its UI state
  // ...
}
```

### 23. Never Store Derived State

If you can compute it from props or other state, compute it.

```typescript
// ✗ BAD - derived state stored
function List({ items, selectedId }: Props) {
  const [selectedItem, setSelectedItem] = useState(
    items.find(i => i.id === selectedId)
  );

  useEffect(() => {
    setSelectedItem(items.find(i => i.id === selectedId));
  }, [items, selectedId]);
}

// ✓ GOOD - derived inline
function List({ items, selectedId }: Props) {
  const selectedItem = items.find(i => i.id === selectedId);
  // ...
}
```

### 24. Controlled vs Uncontrolled

Document and be intentional:

```typescript
interface DrawerProps {
  // Domain state - CONTROLLED (parent owns)
  items: Item[];
  selectedId: string | null;
  onSelect: (id: string) => void;

  // UI state - UNCONTROLLED (component owns)
  // isExpanded, dragPosition, etc. managed internally
}

function Drawer({ items, selectedId, onSelect }: DrawerProps) {
  // UI state - local
  const [isExpanded, setIsExpanded] = useState(true);
  const [height, setHeight] = useState(300);

  // Domain state - from props
  const selectedItem = items.find(i => i.id === selectedId);

  // ...
}
```

---

## Hooks Best Practices

### 25. Custom Hooks for Reusable Logic

Extract stateful logic into custom hooks. Keep components focused on rendering.

```typescript
// ✓ GOOD - logic extracted to hook
function useResizable(initialSize: number, minSize: number, maxSize: number) {
  const [size, setSize] = useState(initialSize);
  const [isDragging, setIsDragging] = useState(false);

  const handleDragStart = useCallback(() => setIsDragging(true), []);
  const handleDragEnd = useCallback(() => setIsDragging(false), []);
  const handleDrag = useCallback((delta: number) => {
    setSize(s => Math.min(maxSize, Math.max(minSize, s + delta)));
  }, [minSize, maxSize]);

  return { size, isDragging, handleDragStart, handleDragEnd, handleDrag };
}

// Component stays focused on rendering
function Panel() {
  const { size, isDragging, handleDragStart, handleDrag, handleDragEnd } =
    useResizable(300, 100, 600);

  return <div style={{ height: size }}>...</div>;
}
```

### 26. Hook Dependency Arrays

Be explicit and complete. Don't lie to React.

```typescript
// ✗ BAD - missing dependency
const handleSubmit = useCallback(() => {
  submitForm(formData); // formData not in deps!
}, []);

// ✗ BAD - eslint-disable to hide the lie
const handleSubmit = useCallback(() => {
  submitForm(formData);
  // eslint-disable-next-line react-hooks/exhaustive-deps
}, []);

// ✓ GOOD - all deps included
const handleSubmit = useCallback(() => {
  submitForm(formData);
}, [formData]);

// ✓ GOOD - if you need stable reference, use ref
const formDataRef = useRef(formData);
formDataRef.current = formData;

const handleSubmit = useCallback(() => {
  submitForm(formDataRef.current);
}, []);
```

### 27. useMemo and useCallback

Don't prematurely optimize. Use when:
- Expensive computations
- Referential equality matters (deps for other hooks, memo'd children)

```typescript
// ✗ BAD - unnecessary memoization
const name = useMemo(() => user.name, [user.name]);
const handleClick = useCallback(() => setOpen(true), []);

// ✓ GOOD - expensive computation
const sortedItems = useMemo(
  () => items.slice().sort((a, b) => complexSort(a, b)),
  [items]
);

// ✓ GOOD - stable reference needed for child memo or effect dep
const handleSelect = useCallback((id: string) => {
  onSelect(id);
  trackAnalytics('selected', id);
}, [onSelect]);
```

### 28. useRef for Non-Render Values

Use ref for values that shouldn't trigger re-renders:

```typescript
function Tracker({ onComplete }: Props) {
  // ✓ Value that shouldn't trigger re-render
  const startTimeRef = useRef(Date.now());
  const attemptCountRef = useRef(0);

  const handleComplete = () => {
    const duration = Date.now() - startTimeRef.current;
    onComplete({ duration, attempts: attemptCountRef.current });
  };

  const handleRetry = () => {
    attemptCountRef.current += 1;  // No re-render
    // ...
  };
}
```

---

## Effects Best Practices

### 29. Effects are for Synchronization

Effects sync React state with external systems. Not for derived state.

```typescript
// ✗ BAD - effect for derived state
useEffect(() => {
  setFullName(`${firstName} ${lastName}`);
}, [firstName, lastName]);

// ✓ GOOD - compute directly
const fullName = `${firstName} ${lastName}`;

// ✓ GOOD - effect for external sync
useEffect(() => {
  const handler = (e: KeyboardEvent) => {
    if (e.key === 'Escape') onClose();
  };
  window.addEventListener('keydown', handler);
  return () => window.removeEventListener('keydown', handler);
}, [onClose]);
```

### 30. Cleanup in Effects

Always clean up subscriptions, timers, listeners:

```typescript
useEffect(() => {
  const subscription = dataSource.subscribe(handleData);
  return () => subscription.unsubscribe(); // Cleanup
}, []);

useEffect(() => {
  const timer = setInterval(tick, 1000);
  return () => clearInterval(timer); // Cleanup
}, []);

useEffect(() => {
  const controller = new AbortController();
  fetchData(id, { signal: controller.signal });
  return () => controller.abort(); // Cleanup
}, [id]);
```

### 31. Avoid Effect Chains

Don't cascade effects. Compute synchronously or consolidate.

```typescript
// ✗ BAD - effect chain
useEffect(() => {
  setA(computeA(input));
}, [input]);

useEffect(() => {
  setB(computeB(a));
}, [a]);

useEffect(() => {
  setC(computeC(b));
}, [b]);

// ✓ GOOD - compute together or use single effect
const a = computeA(input);
const b = computeB(a);
const c = computeC(b);

// Or if truly needs effect:
useEffect(() => {
  const a = computeA(input);
  const b = computeB(a);
  const c = computeC(b);
  setState({ a, b, c });
}, [input]);
```

---

## Async Patterns

### 32. Async in Effects

Handle race conditions and cleanup:

```typescript
useEffect(() => {
  let cancelled = false;

  async function load() {
    setLoading(true);
    try {
      const data = await fetchData(id);
      if (!cancelled) {
        setData(data);
        setError(null);
      }
    } catch (err) {
      if (!cancelled) {
        setError(err);
        setData(null);
      }
    } finally {
      if (!cancelled) {
        setLoading(false);
      }
    }
  }

  load();
  return () => { cancelled = true; };
}, [id]);
```

### 33. AbortController for Fetch

```typescript
useEffect(() => {
  const controller = new AbortController();

  fetch(`/api/items/${id}`, { signal: controller.signal })
    .then(res => res.json())
    .then(data => setData(data))
    .catch(err => {
      if (err.name !== 'AbortError') {
        setError(err);
      }
    });

  return () => controller.abort();
}, [id]);
```

### 34. Loading and Error States

Always handle all states:

```typescript
type AsyncState<T> =
  | { status: 'idle' }
  | { status: 'loading' }
  | { status: 'success'; data: T }
  | { status: 'error'; error: Error };

function useAsync<T>(asyncFn: () => Promise<T>, deps: unknown[]): AsyncState<T> {
  const [state, setState] = useState<AsyncState<T>>({ status: 'idle' });

  useEffect(() => {
    let cancelled = false;
    setState({ status: 'loading' });

    asyncFn()
      .then(data => {
        if (!cancelled) setState({ status: 'success', data });
      })
      .catch(error => {
        if (!cancelled) setState({ status: 'error', error });
      });

    return () => { cancelled = true; };
  }, deps);

  return state;
}
```

### 35. Event Handlers Can Be Async

Unlike effects, event handlers can be async directly:

```typescript
// ✓ GOOD - async event handler
const handleSubmit = async () => {
  setSubmitting(true);
  try {
    await submitForm(formData);
    onSuccess();
  } catch (error) {
    console.error('[Form] Submit failed', { error });
    setError(error);
  } finally {
    setSubmitting(false);
  }
};
```

---

## Forbidden React Patterns

These are **NEVER allowed** unless explicitly requested:

- ❌ **Storing derived state** - Compute from props/state instead
- ❌ **Effect chains for computed values** - Compute synchronously
- ❌ **Missing effect cleanup** - Always clean up subscriptions/timers/listeners
- ❌ **Lying about hook dependencies** - Never omit or use eslint-disable
- ❌ **Class components** - Use functional components only
- ❌ **Premature optimization** - Don't wrap everything in useMemo/useCallback

---

## Testing React Components

See `unit-testing.md` for general testing patterns. React-specific:

### Render and Query

```typescript
import { render, screen } from '@testing-library/react';

it('should display user name', () => {
  render(<UserProfile name="Alice" />);
  
  expect(screen.getByText('Alice')).toBeInTheDocument();
});
```

### User Interactions

```typescript
import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

it('should toggle menu on click', async () => {
  const user = userEvent.setup();
  render(<NavigationMenu />);
  
  const button = screen.getByRole('button', { name: 'Menu' });
  await user.click(button);
  
  expect(screen.getByRole('menu')).toBeVisible();
});
```

### Async Behavior

```typescript
it('should load and display users', async () => {
  render(<UserList />);
  
  // Wait for loading to finish
  expect(await screen.findByText('Alice')).toBeInTheDocument();
  expect(screen.getByText('Bob')).toBeInTheDocument();
});
```

---

## Pre-Commit Checklist

- [ ] Components follow single responsibility principle
- [ ] State is colocated (not lifted unnecessarily)
- [ ] No derived state stored in useState
- [ ] All effects have cleanup functions
- [ ] Hook dependency arrays are complete and honest
- [ ] Custom hooks for reusable stateful logic
- [ ] Props are minimal (only what's needed)
- [ ] Event handlers follow naming convention (onX/handleX)
- [ ] Boolean props use is/has/can/should prefixes
