# Notification System Spec

> Generated: 2024-01-15
> Status: Approved
> Estimated agents: 5

## Overview

Build a multi-channel notification system that sends email, in-app, and push notifications based on user actions and system events. Users can configure their notification preferences per channel and category.

## User Stories

- As a user, I want to receive notifications when someone comments on my post so that I can respond quickly
- As a user, I want to configure which notifications I receive so that I'm not overwhelmed
- As a user, I want to see my unread notifications in-app so that I don't miss important updates
- As an admin, I want to send system-wide announcements so that I can communicate with all users

## Technical Context

### Code Guidelines
- **Language**: See `.claude/guidelines/typescript.md`
- **Unit Testing**: See `.claude/guidelines/unit-testing.md`
- **E2E Testing**: See `.claude/guidelines/e2e-testing.md`

### Existing Patterns to Follow
- API routes: See `src/routes/users.ts` for REST conventions
- Services: See `src/services/auth.service.ts` for service layer pattern
- Database: See `src/db/schema.ts` for Drizzle ORM patterns
- React components: See `app/components/ui/` for component conventions

### Dependencies
- `@react-email/components`: ^0.0.12 - email templates
- `web-push`: ^3.6.0 - push notifications (already installed)
- `@tanstack/react-query`: ^5.0.0 - client data fetching (already installed)

### Constraints
- Must work with existing auth system (JWT)
- Email sending via existing Resend integration
- Push notifications via existing web-push setup
- Real-time updates via existing WebSocket connection

---

## Components

### Component 1: Database Schema & Types

**Purpose**: Define notification data model and TypeScript types

**Files**:
- `src/db/schema/notifications.ts` (create)
- `src/types/notifications.ts` (create)

**Interfaces**:
```typescript
// src/types/notifications.ts

export type NotificationChannel = 'email' | 'in_app' | 'push';
export type NotificationCategory = 'comments' | 'mentions' | 'follows' | 'system';
export type NotificationStatus = 'pending' | 'sent' | 'read' | 'failed';

export interface Notification {
  id: string;
  userId: string;
  channel: NotificationChannel;
  category: NotificationCategory;
  status: NotificationStatus;
  title: string;
  body: string;
  data: Record<string, unknown>; // arbitrary payload (link, entityId, etc.)
  createdAt: Date;
  sentAt: Date | null;
  readAt: Date | null;
}

export interface NotificationPreference {
  userId: string;
  category: NotificationCategory;
  email: boolean;
  inApp: boolean;
  push: boolean;
}

export interface CreateNotificationInput {
  userId: string;
  category: NotificationCategory;
  title: string;
  body: string;
  data?: Record<string, unknown>;
}
```

**Database Schema**:
```typescript
// src/db/schema/notifications.ts

export const notifications = pgTable('notifications', {
  id: uuid('id').primaryKey().defaultRandom(),
  userId: uuid('user_id').notNull().references(() => users.id),
  channel: text('channel', { enum: ['email', 'in_app', 'push'] }).notNull(),
  category: text('category', { enum: ['comments', 'mentions', 'follows', 'system'] }).notNull(),
  status: text('status', { enum: ['pending', 'sent', 'read', 'failed'] }).notNull().default('pending'),
  title: text('title').notNull(),
  body: text('body').notNull(),
  data: jsonb('data').default({}),
  createdAt: timestamp('created_at').notNull().defaultNow(),
  sentAt: timestamp('sent_at'),
  readAt: timestamp('read_at'),
});

export const notificationPreferences = pgTable('notification_preferences', {
  id: uuid('id').primaryKey().defaultRandom(),
  userId: uuid('user_id').notNull().references(() => users.id),
  category: text('category', { enum: ['comments', 'mentions', 'follows', 'system'] }).notNull(),
  email: boolean('email').notNull().default(true),
  inApp: boolean('in_app').notNull().default(true),
  push: boolean('push').notNull().default(true),
}, (table) => ({
  uniqueUserCategory: uniqueIndex('user_category_idx').on(table.userId, table.category),
}));
```

**Acceptance Criteria**:
- [ ] Migration creates both tables with correct types
- [ ] Foreign keys to users table work correctly
- [ ] Unique constraint on (userId, category) for preferences
- [ ] Default values applied correctly

**Test Cases**:
- `should create notification with default status 'pending'`
- `should enforce unique preference per user+category`
- `should cascade delete when user is deleted`

**Edge Cases**:
- User deleted: CASCADE delete notifications and preferences
- Duplicate preference insert: UPSERT behavior

---

### Component 2: Notification Service (Core Logic)

**Purpose**: Business logic for creating and dispatching notifications

**Files**:
- `src/services/notification.service.ts` (create)

**Interfaces**:
```typescript
export class NotificationService {
  // Create notification respecting user preferences
  async notify(input: CreateNotificationInput): Promise<Notification[]>;
  
  // Get user's notifications with pagination
  async getForUser(userId: string, options: { 
    limit?: number; 
    cursor?: string;
    unreadOnly?: boolean;
  }): Promise<{ notifications: Notification[]; nextCursor: string | null }>;
  
  // Mark as read
  async markAsRead(notificationId: string, userId: string): Promise<void>;
  async markAllAsRead(userId: string): Promise<void>;
  
  // Get unread count (for badge)
  async getUnreadCount(userId: string): Promise<number>;
}
```

**Behavior**:
1. When `notify()` is called, fetch user's preferences for that category
2. For each enabled channel, create a notification record with status 'pending'
3. Dispatch to appropriate channel handler (email, push, in-app)
4. Update status to 'sent' or 'failed'
5. For in-app notifications, emit WebSocket event

**Acceptance Criteria**:
- [ ] Respects user preferences per channel
- [ ] Creates separate record per channel
- [ ] Handles partial failures (email fails, push succeeds)
- [ ] Returns all created notifications

**Test Cases**:
- `should create in_app notification when email preference is disabled`
- `should create notifications for all enabled channels`
- `should handle email send failure gracefully`
- `should emit websocket event for in_app notifications`
- `should return empty array if all channels disabled`

**Edge Cases**:
- All channels disabled: Return empty array, log warning
- Email service down: Mark as 'failed', continue with other channels
- User has no preferences: Use defaults (all enabled)

---

### Component 3: Notification API Routes

**Purpose**: REST endpoints for notification operations

**Files**:
- `src/routes/notifications.ts` (create)

**Endpoints**:
```typescript
// GET /api/notifications
// Query: { limit?: number, cursor?: string, unreadOnly?: boolean }
// Response: { notifications: Notification[], nextCursor: string | null }

// GET /api/notifications/unread-count
// Response: { count: number }

// PATCH /api/notifications/:id/read
// Response: { success: true }

// PATCH /api/notifications/read-all
// Response: { success: true }

// GET /api/notifications/preferences
// Response: { preferences: NotificationPreference[] }

// PATCH /api/notifications/preferences
// Body: { category: string, email?: boolean, inApp?: boolean, push?: boolean }
// Response: { preference: NotificationPreference }
```

**Behavior**:
1. All endpoints require authentication (use existing `requireAuth` middleware)
2. Users can only access their own notifications
3. Pagination via cursor (not offset) for efficiency
4. Preferences endpoint creates default if none exist

**Acceptance Criteria**:
- [ ] All endpoints require authentication
- [ ] Users cannot access other users' notifications
- [ ] Cursor pagination works correctly
- [ ] Invalid IDs return 404

**Test Cases**:
- `should return 401 without auth token`
- `should return 404 for notification belonging to other user`
- `should paginate correctly with cursor`
- `should create default preferences on first access`

**Edge Cases**:
- Invalid cursor: Return 400 with helpful message
- Notification not found: Return 404
- Concurrent read updates: Idempotent (no error if already read)

---

### Component 4: React Notification Components

**Purpose**: UI for viewing and managing notifications

**Files**:
- `app/components/notifications/NotificationBell.tsx` (create)
- `app/components/notifications/NotificationList.tsx` (create)
- `app/components/notifications/NotificationItem.tsx` (create)
- `app/components/notifications/NotificationPreferences.tsx` (create)
- `app/hooks/useNotifications.ts` (create)

**Component Specs**:

```typescript
// NotificationBell - shows unread count, opens dropdown
interface NotificationBellProps {
  // No props - uses context/hooks internally
}
// Renders: Bell icon with badge count, dropdown on click

// NotificationList - paginated list in dropdown or page
interface NotificationListProps {
  maxHeight?: number; // for dropdown mode
  showLoadMore?: boolean;
}
// Renders: List of NotificationItem, load more button, empty state

// NotificationItem - single notification
interface NotificationItemProps {
  notification: Notification;
  onRead: (id: string) => void;
}
// Renders: Icon, title, body, timestamp, unread indicator

// NotificationPreferences - settings UI
interface NotificationPreferencesProps {
  // No props - fetches own data
}
// Renders: Grid of toggles per category/channel
```

**Behavior**:
1. NotificationBell polls unread count every 30s (or uses WebSocket)
2. Clicking bell opens dropdown with recent notifications
3. Clicking notification marks as read and navigates if link exists
4. "Mark all read" button at top of list
5. Preferences page shows toggle grid

**Acceptance Criteria**:
- [ ] Unread count updates in real-time via WebSocket
- [ ] Notifications marked read on click
- [ ] Infinite scroll/load more works
- [ ] Empty state shown when no notifications
- [ ] Preferences save immediately on toggle

**Test Cases**:
- `should display unread count badge`
- `should mark notification read on click`
- `should load more notifications on scroll`
- `should show empty state when no notifications`
- `should toggle preference and persist`

**Edge Cases**:
- Network error: Show retry button, don't lose state
- Rapid toggles: Debounce preference updates
- Very long notification text: Truncate with ellipsis

---

### Component 5: Email Templates

**Purpose**: React Email templates for notification emails

**Files**:
- `src/emails/NotificationEmail.tsx` (create)
- `src/emails/templates/comment.tsx` (create)
- `src/emails/templates/mention.tsx` (create)
- `src/emails/templates/follow.tsx` (create)
- `src/emails/templates/system.tsx` (create)

**Template Structure**:
```typescript
// Base template with shared layout
interface NotificationEmailProps {
  userName: string;
  title: string;
  body: string;
  actionUrl?: string;
  actionText?: string;
}

// Each category template extends base with specific styling/content
```

**Acceptance Criteria**:
- [ ] All templates render valid HTML email
- [ ] Responsive design (mobile-friendly)
- [ ] Unsubscribe link included
- [ ] Preview text set correctly

**Test Cases**:
- `should render comment notification email`
- `should include unsubscribe link`
- `should handle missing actionUrl gracefully`

**Edge Cases**:
- Very long body: Truncate at 500 chars with "Read more" link
- Special characters in title: Properly escaped

---

## Integration Points

### Between Components
- Component 2 (Service) imports types from Component 1
- Component 3 (Routes) uses Component 2 (Service)
- Component 4 (React) calls Component 3 (API)
- Component 2 (Service) uses Component 5 (Email) templates

### External Services
- **Resend** (email): Use existing `src/lib/email.ts` client
- **WebSocket**: Emit to existing `src/lib/websocket.ts` server
- **Web Push**: Use existing `src/lib/push.ts` utilities

## Data Flow

```
[User action: new comment]
    ↓
[Event emitter: 'comment.created']
    ↓
[NotificationService.notify()]
    ↓
[Check user preferences]
    ↓
[Create notification records]
    ↓
├─→ [Email channel] → Resend API → User inbox
├─→ [Push channel] → Web Push API → User device
└─→ [In-app channel] → WebSocket → NotificationBell updates
```

## Non-Functional Requirements

- **Performance**: Unread count query < 50ms (indexed)
- **Reliability**: Email failures retried 3x with exponential backoff
- **Security**: Users can only see own notifications (row-level)

## Out of Scope

- Notification scheduling (send later) - separate task
- Notification templates admin UI - separate task
- SMS channel - future enhancement
- Notification grouping/digest - future enhancement

## Open Questions

None - all resolved during planning.

## Decomposition Hint

| Agent | Component | Files | Can Parallel With |
|-------|-----------|-------|-------------------|
| 1 | DB Schema & Types | db/schema/notifications.ts, types/notifications.ts | 5 |
| 2 | Notification Service | services/notification.service.ts | After 1 |
| 3 | API Routes | routes/notifications.ts | After 2 |
| 4 | React Components | components/notifications/*, hooks/* | After 3 |
| 5 | Email Templates | emails/* | 1, 2, 3 |

**Execution order**:
1. Agents 1 + 5 in parallel (no dependencies)
2. Agent 2 after Agent 1 completes
3. Agent 3 after Agent 2 completes
4. Agent 4 after Agent 3 completes

Total parallelism: 2 agents max at any time for this spec.
