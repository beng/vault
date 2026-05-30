# Example Spec: Task Management API

> This is an example spec formatted for easy parallel decomposition.
> Notice how each component maps cleanly to separate files.

## Overview

Build a task management REST API with user authentication, task CRUD, and team collaboration.

## Components

### 1. Database Schema & Models

**Files**: `src/db/schema.ts`, `src/models/*.ts`

- Users table (id, email, password_hash, name, created_at)
- Tasks table (id, title, description, status, assignee_id, team_id, due_date)
- Teams table (id, name, owner_id)
- Team_members junction table (team_id, user_id, role)

### 2. Authentication System

**Files**: `src/auth/*.ts`, `src/middleware/auth.ts`

- POST /auth/register - create account
- POST /auth/login - get JWT token
- POST /auth/logout - invalidate token
- POST /auth/refresh - refresh token
- Middleware: requireAuth, optionalAuth

### 3. User Management

**Files**: `src/routes/users.ts`, `src/services/user.service.ts`

- GET /users/me - current user profile
- PATCH /users/me - update profile
- GET /users/:id - view user (public info only)

### 4. Task CRUD

**Files**: `src/routes/tasks.ts`, `src/services/task.service.ts`

- GET /tasks - list tasks (with filters: status, assignee, team)
- POST /tasks - create task
- GET /tasks/:id - get task detail
- PATCH /tasks/:id - update task
- DELETE /tasks/:id - delete task

### 5. Team Management

**Files**: `src/routes/teams.ts`, `src/services/team.service.ts`

- GET /teams - list user's teams
- POST /teams - create team
- GET /teams/:id - team detail
- POST /teams/:id/members - add member
- DELETE /teams/:id/members/:userId - remove member

### 6. Shared Utilities

**Files**: `src/utils/*.ts`

- Error handling utilities
- Validation helpers (zod schemas)
- Date formatting
- Pagination helpers

### 7. Tests

**Files**: `src/__tests__/*.test.ts`

Each component should have corresponding tests:
- auth.test.ts
- users.test.ts
- tasks.test.ts
- teams.test.ts

## Technical Requirements

- Framework: Express.js or Fastify
- Database: PostgreSQL with Drizzle ORM
- Auth: JWT with refresh tokens
- Validation: Zod
- Testing: Vitest

## Decomposition Notes

This spec is designed for 6 parallel agents:

| Agent | Component | Files | Dependencies |
|-------|-----------|-------|--------------|
| 1 | Database & Models | db/, models/ | None (run first) |
| 2 | Auth System | auth/, middleware/ | Models |
| 3 | User Management | routes/users, services/user | Auth |
| 4 | Task CRUD | routes/tasks, services/task | Models |
| 5 | Team Management | routes/teams, services/team | Models |
| 6 | Shared Utils | utils/ | None (can run parallel) |

**Execution order:**
1. Run Agent 1 (models) + Agent 6 (utils) in parallel
2. When models done, run Agents 2-5 in parallel
3. Each agent writes their own tests

**File boundaries are clear** - no two agents touch the same file.
