# Python Guidelines (Python 3.12)

You are working in a Python 3.12 codebase. When you generate or modify code or tests, follow these rules:

## Code Style

### Formatting
- Use `ruff` for linting and formatting (replaces black, isort, flake8)
- Line length: 88 characters
- Use double quotes for strings
- Always run `ruff format . && ruff check --fix .` before committing

### Type Hints (Mandatory)

- Code must be **fully typed**: every function, method, variable, and return value uses explicit types.
- Target **Python 3.12** and its best practices.

```python
# ✅ Good - fully typed
def get_user(user_id: str) -> User | None:
    ...

async def create_user(data: CreateUserInput) -> User:
    ...

def process_items(items: list[Item]) -> dict[str, int]:
    ...

# ❌ Bad - no types
def get_user(user_id):
    ...
```

### Imports
```python
# Standard library
from collections.abc import Callable
from dataclasses import dataclass
from typing import Any

# Third-party
import httpx
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

# Local
from src.models.user import User
from src.services.user_service import UserService
```

### Naming Conventions
```python
# Classes: PascalCase
class UserService:
    pass

# Functions/methods: snake_case
def get_active_users() -> list[User]:
    pass

# Constants: UPPER_SNAKE_CASE
MAX_RETRY_ATTEMPTS = 3
DEFAULT_TIMEOUT = 30

# Private: single underscore prefix
def _internal_helper() -> None:
    pass

# Module-level "constants" that are mutable: lowercase
default_config = Config()
```

## General Code Quality

- Favor **small, composable functions** over large classes. Keep functions single-responsibility, pure where possible, and easy to test.
- Prefer **type composition and functional composition** over heavy OOP.
  - Use simple functions + data structures first.
  - OOP (classes, inheritance, complex hierarchies) is **less preferred** and should only be used when clearly justified.
- Keep modules cohesive and avoid god-objects or "manager" classes that do everything.

## Data Models

### Pydantic Models (API boundaries)

For data structures, **prefer Pydantic models** to define schemas and types (v2-style if relevant).

```python
from pydantic import BaseModel, Field, field_validator

class CreateUserInput(BaseModel):
    """Input for creating a user."""
    
    email: str = Field(..., description="User's email address")
    name: str = Field(..., min_length=1, max_length=100)
    age: int | None = Field(default=None, ge=0, le=150)
    
    @field_validator("email")
    @classmethod
    def validate_email(cls, v: str) -> str:
        if "@" not in v:
            raise ValueError("Invalid email format")
        return v.lower()
    
    model_config = {"str_strip_whitespace": True}


class UserResponse(BaseModel):
    """User data returned from API."""
    
    id: str
    email: str
    name: str
    created_at: datetime
    
    model_config = {"from_attributes": True}
```

### Dataclasses (Internal use)
```python
from dataclasses import dataclass, field
from datetime import datetime

@dataclass
class User:
    id: str
    email: str
    name: str
    created_at: datetime = field(default_factory=datetime.utcnow)
    
    def is_active(self) -> bool:
        return self.status == "active"
```

## Interfaces & Design Patterns

- For common interfaces, **use `typing.Protocol` first**.
  - Example: repository interfaces, services, handlers.

- **`abc.ABC`** is a **fallback** when we truly need abstract base classes (e.g., shared logic + abstract hooks).
- Aim for:
  - **Explicit, narrow interfaces**.
  - **Dependency injection** over hard-coded dependencies.
  - **Stateless, side-effect-light functions** where possible.

### Dependency Injection

```python
# FastAPI style - preferred
from fastapi import Depends

def get_user_service(
    repo: UserRepository = Depends(get_user_repository),
    cache: Cache = Depends(get_cache),
) -> UserService:
    return UserService(repo, cache)

@router.get("/users/{user_id}")
async def get_user(
    user_id: str,
    service: UserService = Depends(get_user_service),
) -> UserResponse:
    return await service.get_user(user_id)
```

## Error Handling

### Core Principles

- **Never silently fail.**
  - Do **not** swallow exceptions without logging.
  - If you catch an exception, **log it** and then **re-raise** or wrap it in a more specific exception.
- Always **bubble up exceptions** to a layer that can act on them (e.g., API or job runner), rather than hiding them.

### Custom Exceptions
```python
# src/exceptions.py

class AppError(Exception):
    """Base exception for application errors."""
    
    def __init__(self, message: str, code: str | None = None):
        self.message = message
        self.code = code
        super().__init__(message)


class NotFoundError(AppError):
    """Resource not found."""
    pass


class ValidationError(AppError):
    """Invalid input data."""
    pass


class ExternalServiceError(AppError):
    """External API/service failure."""
    pass
```

### Error Handling Patterns

```python
import logging

logger = logging.getLogger(__name__)

# ✓ GOOD - log and re-raise
def process_data(data_id: str) -> ProcessedData:
    try:
        raw_data = fetch_data(data_id)
        return transform(raw_data)
    except DataFetchError as e:
        logger.error("Failed to fetch data", extra={"data_id": data_id, "error": str(e)})
        raise  # Re-raise so caller can handle
    except TransformError as e:
        logger.error("Transform failed", extra={"data_id": data_id, "error": str(e)})
        raise ProcessingError(f"Could not process {data_id}") from e

# ✗ BAD - silent failure
def process_data_bad(data_id: str) -> ProcessedData | None:
    try:
        raw_data = fetch_data(data_id)
        return transform(raw_data)
    except Exception:
        return None  # ❌ Error is hidden!


# ✅ Good - specific exceptions, proper logging
from src.exceptions import NotFoundError, ExternalServiceError

async def get_user(user_id: str) -> User:
    try:
        user = await repository.find_by_id(user_id)
    except DatabaseError as e:
        logger.error(f"Database error fetching user {user_id}: {e}")
        raise ExternalServiceError("Database unavailable") from e
    
    if user is None:
        raise NotFoundError(f"User {user_id} not found")
    
    return user
```

## Logging

- **We intentionally over-log.** Debuggability is critical.
- Log key events, decisions, and error conditions with enough context (ids, parameters, state summaries) to debug production issues.
- Prefer structured logging where possible (key-value style).

```python
import structlog

# Configure once at startup
structlog.configure(
    processors=[
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.JSONRenderer(),
    ],
)

# Usage
logger = structlog.get_logger()

async def create_user(data: CreateUserInput) -> User:
    logger.info("creating_user", email=data.email)
    
    try:
        user = await repository.create(data)
        logger.info("user_created", user_id=user.id)
        return user
    except Exception as e:
        logger.error("user_creation_failed", error=str(e), email=data.email)
        raise
```

## Async Patterns

### Async Functions
```python
import asyncio
from collections.abc import AsyncIterator

# Prefer async for I/O operations
async def fetch_users(ids: list[str]) -> list[User]:
    tasks = [fetch_user(id) for id in ids]
    return await asyncio.gather(*tasks)

# Async context managers
async def get_db_connection() -> AsyncIterator[Connection]:
    conn = await pool.acquire()
    try:
        yield conn
    finally:
        await pool.release(conn)

# Async iteration
async def stream_results() -> AsyncIterator[Result]:
    async for row in cursor:
        yield Result.from_row(row)
```

## Testing (Pytest)

When you write tests, assume we use **pytest** everywhere.

- Follow **pytest best practices**:
  - Use **`@pytest.mark.parametrize`** for variations in inputs/outputs instead of duplicating tests.
  - Use **`@pytest.fixture`** for shared setup instead of copy-pasting boilerplate.

- Tests should be:
  - **Deterministic**, **isolated**, and **fast**.
  - Focused on behavior and observable outputs.

- Avoid redundant tests. Prefer a small, well-parameterized test suite over many copy-pasted cases.

### Example: Parameterized Test

```python
import pytest

@pytest.mark.parametrize("input,expected", [
    ("hello", "HELLO"),
    ("world", "WORLD"),
    ("", ""),
])
def test_uppercase(input: str, expected: str) -> None:
    assert input.upper() == expected
```

### Example: Fixture for Setup

```python
import pytest

@pytest.fixture
def sample_data() -> list[dict[str, Any]]:
    """Shared test data for multiple tests."""
    return [
        {"id": 1, "name": "Alice"},
        {"id": 2, "name": "Bob"},
    ]

def test_filter_by_id(sample_data: list[dict[str, Any]]) -> None:
    result = [item for item in sample_data if item["id"] == 1]
    assert len(result) == 1
    assert result[0]["name"] == "Alice"
```

## Configuration

```python
# src/config.py
from pydantic_settings import BaseSettings, SettingsConfigDict

class Settings(BaseSettings):
    """Application settings loaded from environment."""
    
    database_url: str
    redis_url: str = "redis://localhost:6379"
    debug: bool = False
    api_key: str
    
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
    )

# Usage
settings = Settings()
```

## Comments & Documentation

Follow the universal comment principles from CLAUDE.md. Here are Python-specific examples:

### Good Python Comments

```python
def reconcile_events(
    local_events: list[Event],
    remote_events: list[Event]
) -> list[Event]:
    """
    Merge local and remote event streams, preferring remote state on conflict.

    Assumes both lists are sorted by timestamp ascending.
    Used by sync pipeline; changes here affect offline conflict resolution.
    """
    # ...implementation...
```

```python
# Single source of truth for event status -> visual state mapping.
# Backend and diagnostics both rely on these four states; keep in sync.
STATUS_TO_STATE: dict[EventStatus, VisualState] = {
    EventStatus.PENDING: VisualState.WAITING,
    EventStatus.ACTIVE: VisualState.IN_PROGRESS,
    EventStatus.COMPLETED: VisualState.SUCCESS,
    EventStatus.FAILED: VisualState.ERROR,
}
```

### Docstring Format

```python
def calculate_discount(
    price: float,
    discount_percent: float,
    max_discount: float | None = None,
) -> float:
    """Calculate discounted price.
    
    Args:
        price: Original price in dollars.
        discount_percent: Discount as percentage (0-100).
        max_discount: Maximum discount amount. If None, no limit.
    
    Returns:
        Final price after discount.
    
    Raises:
        ValueError: If discount_percent is not between 0 and 100.
    
    Example:
        >>> calculate_discount(100.0, 20.0)
        80.0
        >>> calculate_discount(100.0, 50.0, max_discount=30.0)
        70.0
    """
    if not 0 <= discount_percent <= 100:
        raise ValueError(f"Invalid discount: {discount_percent}")
    
    discount = price * (discount_percent / 100)
    if max_discount is not None:
        discount = min(discount, max_discount)
    
    return price - discount
```

### Bad Python Comments

```python
# ❌ Restates the code
# Loop through items
for item in items:
    # Process each item
    process(item)

# ❌ Decorative noise
# ==================
# HELPER FUNCTIONS
# ==================

# ❌ Obvious
# Increment counter
counter += 1
```

## Anti-Patterns to Avoid

```python
# ❌ Mutable default arguments
def add_item(item, items=[]):  # Bug: shared list!
    items.append(item)
    return items

# ✅ Correct
def add_item(item, items: list | None = None):
    if items is None:
        items = []
    items.append(item)
    return items


# ❌ Using type() for type checking
if type(x) == str:
    ...

# ✅ Use isinstance
if isinstance(x, str):
    ...


# ❌ Catching too broad
try:
    do_something()
except Exception:
    pass

# ✅ Catch specific exceptions
try:
    do_something()
except (ValueError, KeyError) as e:
    logger.warning(f"Expected error: {e}")


# ❌ String concatenation in loops
result = ""
for item in items:
    result += str(item)

# ✅ Use join
result = "".join(str(item) for item in items)
```

## When in Doubt

If there's any tradeoff, choose the option that:

1. Improves **readability** and **maintainability**.
2. Keeps **types and contracts explicit**.
3. Makes **testing and debugging easier** (clear logs, clear exceptions).
4. Avoids unnecessary OOP complexity.

Always explain your design choices in comments or docstrings when they are non-obvious.

## Forbidden Patterns

These are **NEVER allowed** in Python code unless explicitly requested:

- ❌ **Silent failures**: Catching exceptions without logging and re-raising
- ❌ **Missing type hints**: Every function, method, variable must be typed
- ❌ **God objects**: Classes that do everything; prefer small, focused functions
- ❌ **Swallowing exceptions**: `except Exception: pass` or returning None on error without logging
- ❌ **Opaque error messages**: Exceptions without context (ids, state, parameters)
- ❌ **Mutable default arguments**: Use `None` and create new instances inside function
- ❌ **Bare except clauses**: Always catch specific exceptions
- ❌ **Using `type()` for type checks**: Use `isinstance()` instead

## Required Tools

```bash
# Install dev dependencies
pip install ruff mypy pytest pytest-asyncio

# Validate before commit
ruff format .
ruff check --fix .
mypy src/
pytest
```

## Pre-Commit Checklist

- [ ] All functions have type hints
- [ ] `ruff format .` produces no changes
- [ ] `ruff check .` shows no errors
- [ ] `mypy src/` shows no errors
- [ ] All tests pass
- [ ] Docstrings on public functions
