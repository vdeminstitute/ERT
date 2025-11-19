# Frequently Asked Questions (FAQ)

## General Linux Questions

### In linux how can I see a list of all users?

In Linux, you can see a list of all users using several methods:

#### Method 1: Using the /etc/passwd file
The most common way is to view the `/etc/passwd` file, which contains information about all user accounts:

```bash
cat /etc/passwd
```

This will display all users, but includes system users as well. Each line represents a user account.

To see just the usernames:
```bash
cut -d: -f1 /etc/passwd
```

#### Method 2: Using getent command
The `getent` command queries system databases:

```bash
getent passwd
```

To list only usernames:
```bash
getent passwd | cut -d: -f1
```

#### Method 3: List only human users (UID >= 1000)
To filter out system users and show only regular user accounts:

```bash
awk -F: '$3 >= 1000 {print $1}' /etc/passwd
```

Or:
```bash
getent passwd | awk -F: '$3 >= 1000 {print $1}'
```

#### Method 4: List currently logged in users
To see who is currently logged in:

```bash
who
```

Or:
```bash
w
```

#### Method 5: List users who can login
To see users with a valid login shell (not `/sbin/nologin` or `/bin/false`):

```bash
grep -vE '(nologin|false)' /etc/passwd | cut -d: -f1
```

### Additional Information

- System users (like `daemon`, `bin`, `sys`) typically have UIDs below 1000
- Regular user accounts typically start at UID 1000 (or 500 on some older systems)
- The `/etc/passwd` file format: `username:password:UID:GID:comment:home:shell`
