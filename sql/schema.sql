CREATE TABLE projects (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT,
  description TEXT
);

CREATE TABLE releases (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT,
  description TEXT,
  project_id INTEGER,
  FOREIGN KEY(project_id) REFERENCES projects(id)
);

CREATE TABLE issues (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT,
  description TEXT,
  release_id INTEGER,
  FOREIGN KEY(release_id) REFERENCES releases(id)
);

CREATE TABLE records (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  comments TEXT,
  description TEXT,
  started_on INTEGER,
  finished_on INTEGER,
  FOREIGN KEY(issue_id) REFERENCES issues(id)
);
