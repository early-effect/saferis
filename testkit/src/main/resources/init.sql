-- Create a simple test table no keys defined
CREATE TABLE test_table_no_key (
    name VARCHAR(100) NOT NULL,
    age INT,
    email VARCHAR(100)
);

INSERT INTO test_table_no_key (name, age, email) VALUES
('Alice', 30, 'alice@example.com'),
('Bob', 25, 'bob@example.com'),
('Charlie', null, 'charlie@example.com'),
('Diana', 28, null);

-- Create a simple test table with a primary key - non-generated
CREATE TABLE test_table_primary_key (
    id INT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    age INT,
    email VARCHAR(100)
);

INSERT INTO test_table_primary_key (id, name, age, email) VALUES
(1, 'Alice', 30, 'alice@example.com'),
(2, 'Bob', 25, 'bob@example.com'),
(3, 'Charlie', null, 'charlie@example.com'),
(4, 'Diana', 28, null);

-- Create a simple test table with a primary key - generated
CREATE TABLE test_table_primary_key_generated (
    id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name VARCHAR(100) NOT NULL,
    age INT,
    email VARCHAR(100)
);

INSERT INTO test_table_primary_key_generated (name, age, email) VALUES
('Alice', 30, 'alice@example.com'),
('Bob', 25, 'bob@example.com'),
('Charlie', null, 'charlie@example.com'),
('Diana', 28, null);

-- Create a simple test table with a composite primary key
CREATE TABLE test_table_composite_primary_key (
    id INT,
    name VARCHAR(100) NOT NULL,
    age INT,
    email VARCHAR(100),
    PRIMARY KEY (id, name)
);

INSERT INTO test_table_composite_primary_key (id, name, age, email) VALUES
(1, 'Alice', 30, 'alice@example.com'),
(2, 'Bob', 25, 'bob@example.com'),
(3, 'Charlie', null, 'charlie@example.com'),
(4, 'Diana', 28, null);


