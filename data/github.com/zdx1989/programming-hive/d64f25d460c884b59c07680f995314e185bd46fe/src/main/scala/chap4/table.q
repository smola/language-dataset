DROP DATABASE IF EXISTS mydb;
CREATE DATABASE IF NOT EXISTS mydb;

DROP TABLE IF EXISTS mydb.employees;
CREATE TABLE IF NOT EXISTS mydb.employees (
    name STRING COMMENT 'employee name',
    salary STRING COMMENT 'employee salary',
    subordinates ARRAY<STRING> COMMENT 'names of subordinates',
    deductions Map<String, float> COMMENT '',
    address STRUCT<street: STRING, city: STRING, state: STRING, zip: INT> COMMENT 'home address'
)
COMMENT 'DESCRIPTION OF TABLE'
LOCATION '/user/hive/warehouse/mydb.db/employees';

CREATE TABLE IF NOT EXISTS mydb.employees2
LIKE mydb.employees;

--用户拷贝一张表的模式，而不拷贝表的数据

SHOW TABLES IN mydb;

--列出databases下面所有的表

SHOW TABLES  'empl.*';

-- 可以使用正则表达式来适配表名

DESCRIBE FORMATTED mydb.employees;

--  可以输出表的详细信息

-- 内部表，管理表，存储的路径为/user/hive/warehouse/xx.db/xxx，有hive控制器数据的生命周期，删除表的同时，会删除表中的数据
-- 外部表，hive和其他的工具例如pig共享的数据源，可以为表指定LOCATION，删除外部表，只是删除了表的元数据，表中的数据不会被删除

DROP TABLE IF EXISTS stocks;
CREATE EXTERNAL TABLE IF NOT EXISTS stocks (
    `exchange`  STRING,
    symbol     STRING,
    ymd        STRING,
    price_open FLOAT,
    price_high FLOAT,
    price_low  FLOAT,
    price_close FLOAT,
    volume      INT,
    price_adj_close FLOAT
)
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
LOCATION '/data/stocks';

CREATE TABLE IF NOT EXISTS mydb.employees3
LIKE mydb.employees
LOCATION '/path/to/data'

--复制外部表的模式，但是不会复制表的数据

DROP TABLE IF EXISTS employees;
CREATE TABLE IF NOT EXISTS employees (
    name         STRING,
    salary       FLOAT,
    subordinates ARRAY<STRING>,
    deductions   MAP<STRING, FLOAT>,
    address      STRUCT<street: STRING, city: STRING, state: STRING, zip: INT>
)
PARTITIONED BY (country STRING, state STRING)
ROW FORMAT DELIMITED FIELDS TERMINATED BY ',';

-- 表分区可以缩小查询时数据集的范围，表分区改变数据的存储目录，对应的分区数据存储到对应的分区目录下

-- 分区字段创建好之后，在一般的情况下和普通字段并没有什么区别

SELECT * FROM employees
WHERE country = 'US' AND state = 'IL';

-- 分区字段的数据不会保存在表目录下对应的文件中, 分区目录可以表述分区字段

-- 分区可以缩小查询的数据集，对于大数据查询可以大大的提高性能

-- 将hive设置成严格模式，对于分区表查询假如在where过滤中中没有添加分区过滤的话，将会禁止提交查询任务

set hive.mapred.mode=strict;
SELECT e.name, e.salary FROM employees e LIMIT 100;

set hive.mapred.mode=nostrict;
SELECT e.name, e.salary FROM employees e LIMIT 100;

SHOW PARTITIONS employees;

-- 查看表中的所有分区

SHOW PARTITIONS employees PARTITION(country='CA');

-- 查看表中某个分区字段下所有的分区

-- 可以通过载入数据方式创建分区

LOAD DATA LOCAL INPATH '/Users/zhoudunxiong/Code/programming-hive/src/main/scala/chap4/data.txt'
INTO TABLE employees
PARTITION(country = 'CN', state = 'HN');

-- 分区和分区目录的关系，直接在表目录下添加分区目录，并没有在hive的元数据中添加分区信息，所以show partitions找不到表对应的分区
-- 通过alter table add partition 可以在hive的元数据中添加表的分区信息，同时会在表的目录下添加分区目录

-- 为外部表添加分区

CREATE EXTERNAL TABLE IF NOT EXISTS log_messages (
    hms INT,
    severity STRING,
    server STRING,
    process_id INT,
    message STRING
)
PARTITIONED BY (year INT, month INT, day INT)
ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t';

-- 分区外部表可以在创建的时候不制定LOCATION ？

ALTER TABLE log_messages ADD PARTITION (year = 2018, month = 12, day = 25)
LOCATION '/user/zhoudunxiong/log_messages';

-- 同一张分区外部表，不同的分区可以指向不同的LOCATION
-- hive不关心分区目录是否存在或者分区目录下是否存在文件r

DESCRIBE EXTENDED log_messages PARTITION(year = 2018, month = 12, day = 25);
DESCRIBE FORMATTED log_messages PARTITION(year = 2018, month = 12, day = 25);

-- DESCRIBE EXTENDED log_messages，DESCRIBE FORMATTED log_messages都不会显现外部分区表具体的LOCATION
-- 外部分区表不同的分区可以有不同的LOCATION，所以可以在后面添加具体的partition来显示具体partition的LOCATION

-- 输入流 -> inputFormat -> 记录 -> SerDe -> 列 -> SerDe -> 记录 -> outputFormat -> 输出流

-- 用户可以自己定义第三方SerDe，也可以自己定义第三方InputFormat和OutputFormat

-- 删除表

DROP TABLE IF EXISTS employees;

-- 对于管理表 manage table 删除表会删除表的元数据以及表中的数据
-- 对于外部表 external table 删除表只会删除表的元数据，不会删除表中的数据

-- 对标进行重命名

ALTER TABLE log_messages RENAME TO log;

-- 修改、增加和删除表分区

ALTER TABLE log DROP IF EXISTS PARTITION(year = 2018);

-- 修改列的信息

ALTER TABLE log
CHANGE COLUMN hms hours_minutes_seconds INT
FIRST;

-- 增加列

ALTER TABLE log ADD COLUMNS(
    app_name STRING,
    session_id BIGINT);


-- 可以将分区下面的文件压缩成一个Hadoop压缩包（HAR）文件，这样可以减少文件系统中的文件数，减轻NameNode的压力

-- 新版本的hive不再支持该功能

ALTER TABLE employees ARCHIVE PARTITION (country = 'CN', state = 'HN');

