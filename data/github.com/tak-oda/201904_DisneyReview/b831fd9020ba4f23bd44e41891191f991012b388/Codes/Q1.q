DROP TABLE IF EXISTS review;
CREATE EXTERNAL TABLE IF NOT EXISTS review
(
marketplace string, 
  customer_id string, 
  review_id string, 
  product_id string, 
  product_parent string, 
  product_title string, 
  product_category string,
  star_rating int, 
  helpful_votes int, 
  total_votes int, 
  vine string, 
  verified_purchase string, 
  review_headline string, 
  review_body string, 
  review_date string)
ROW FORMAT DELIMITED FIELDS 
TERMINATED BY '\t' 
LOCATION '/user/maria_dev/final/amazon';


select
    t3.product_category,
    t3.character,
    t3.rank,
    t3.num_reviews
from
(
    select
        t2.product_category,
        t2.character,
        t2.num_reviews,
        dense_rank() over(partition by t2.product_category order by t2.num_reviews desc) rank
    from
    (
        select
            t1.product_category,
            t1.character,
            count(*) num_reviews
        from
            (
            select
                product_category,
                regexp_extract(lower(product_title), 
                '(mickey|donald duck|goofy|minnie|pluto|winnie the pooh|snow white|jack sparrow|tinkerbell|stitch| nala|oswald the lucky rabbit|ariel|baymax|cinderella| belle|alice in wonder lbuzz lightyear|rapunzeldaisy)'
                ,1) character

            from
                review

            ) t1

        where 
            length(t1.character) > 0
        group by
            t1.product_category,
            t1.character
    ) t2
) t3
where
    t3.rank <= 10
order by
    t3.product_category,
    t3.rank