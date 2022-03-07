CREATE DATABASE shoppingcart;
USE shoppingcart;
CREATE TABLE customers(customer_id VARCHAR(10) PRIMARY KEY, 
				customer_name VARCHAR(50) NOT NULL,
                gender VARCHAR(30) NOT NULL,
                age int NOT NULL,
                home_address VARCHAR(100) NOT NULL,
                zip_code int NOT NULL,
                city VARCHAR(30) NOT NULL,
                state VARCHAR(30) NOT NULL,
                country VARCHAR(10) NOT NULL);

CREATE TABLE orders(order_id VARCHAR(10) PRIMARY KEY,
					customer_id VARCHAR(10) NOT NULL,
                    payment int NOT NULL,
                    order_date datetime NOT NULL,
                    delivery_date datetime NOT NULL);

CREATE TABLE products(product_ID VARCHAR(10) PRIMARY KEY,
					  product_type VARCHAR(20) NOT NULL,
                      product_name VARCHAR(50) NOT NULL,
                      size VARCHAR(10) NOT NULL,
                      colour VARCHAR(10) NOT NULL,
                      price int NOT NULL,
                      quantity int NOT NULL,
                      description VARCHAR(100) NOT NULL);

CREATE TABLE sales(sales_ID VARCHAR(10) PRIMARY KEY,
				   order_id VARCHAR(10) NOT NULL,
                   product_id VARCHAR(10) NOT NULL,
                   price_per_unit int NOT NULL,
                   quantity int NOT NULL,
                   total_price int NOT NULL);

ALTER TABLE orders CHANGE order_date order_date date;
ALTER TABLE orders CHANGE delivery_date delivery_date date;

SELECT * FROM customers LIMIT 5;
SELECT * FROM orders;
SELECT * FROM products LIMIT 5;
SELECT * FROM sales LIMIT 5;

-- Q1
SELECT COUNT(customer_id) AS 'Number of customers', State
FROM customers
GROUP BY state
ORDER BY 1 DESC;

-- Q2
SELECT  Gender, count(customer_id) as 'Count', (count(customer_id)/1000)*100 as 'Ratio'
FROM customers
GROUP BY gender
ORDER BY 3 DESC;

-- Q3
SELECT min(age), max(age)
FROM customers;

select count(customer_id) as 'The number of customer', value_range as 'Age Group'
from (select customer_id, age,
            case when age between 20 and 29 then 'Age_20'
                when age between 30 and 39 then 'Age_30'
                when age between 40 and 49 then 'Age_40'
                when age between 50 and 59 then 'Age_50'
                when age between 60 and 69 then 'Age_60'
                when age between 70 and 80 then 'Age_70'
            end as value_range
       from customers) age
group by value_range
ORDER BY 1 DESC;

SELECT *,
CASE
    WHEN 20 < age and age < 25 THEN 'Back to school promotion'
    WHEN 26 < age and age < 60 THEN 'Smart casual in office promotion'
    ELSE  'No Promotion' 
END AS promotion
FROM customers;

-- Q4

SELECT avg(delivery_date - order_date) as 'Average Delivery', min(delivery_date - order_date) as 'Fastest Delivery', max(delivery_date - order_date) as 'Slowest Delivery'
FROM orders;

SELECT order_id, customer_id, delivery_date - order_date as 'Difference'
FROM orders
WHERE delivery_date - order_date = 99 or delivery_date - order_date = 1
ORDER BY 3 DESC
LIMIT 10;

-- Q5

SELECT product_type, min(price), max(price), avg(price)
FROM products
GROUP BY product_type
ORDER BY 4 DESC;

SELECT size, sum(price*quantity) as 'Revenue'
FROM products
GROUP BY size
ORDER BY 1 DESC;

SELECT product_type, product_name, sum(price*quantity) as 'Item revenue'
FROM products
GROUP BY product_name
ORDER BY 3 DESC
LIMIT 5;

SELECT sum(total_price) as total_revenue
FROM sales;
;

-- Q6
SELECT s.order_id, n.customer_id, n.customer_name, sum(total_price) as 'total sale'
FROM sales as s
LEFT join(SELECT o.order_id, o.customer_id, c.customer_name, c.city, c.state
			FROM orders as o
			LEFT JOIN customers as c
			ON o.customer_id = c.customer_id) as n
ON n.order_id = s.order_id
GROUP BY n.customer_id
ORDER BY 4 DESC
LIMIT 5;

-- Q7
SELECT state, sum(total_price) as 'State Revenue'
FROM sales as s
LEFT join(SELECT o.order_id, o.customer_id, c.customer_name, c.city, c.state
			FROM orders as o
			LEFT JOIN customers as c
			ON o.customer_id = c.customer_id) as n
ON n.order_id = s.order_id
GROUP BY n.state
ORDER BY 2 DESC;

SELECT DISTINCT n.customer_name, n.home_address, n.city, n.state
FROM sales as s
LEFT join(SELECT o.order_id, o.customer_id, c.customer_name, c.city, c.home_address, c.state
			FROM orders as o
			LEFT JOIN customers as c
			ON o.customer_id = c.customer_id) as n
ON n.order_id = s.order_id
WHERE n.state = 'Northern Territory';
