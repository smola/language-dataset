
-- Create function for month difference. Can be refined.
drop function months_between;
create function months_between(start_date timestamp, end_date timestamp) RETURNS mb integer
language SQLSCRIPT READS SQL DATA AS
begin
   	mb := (year(:end_date) - year(:start_date))*12 + (month(:end_date) - month(:start_date));
end;
 
-- width_bucket definition.
drop function width_bucket;
create function width_bucket(val Double, start_val Double, end_val Double, nb_buckets integer) RETURNS bucket integer
language SQLSCRIPT READS SQL DATA AS
begin
   	DECLARE b integer := floor((:val - :start_val) * (:nb_buckets / (:end_val - :start_val))) + 1;
   	IF b > :nb_buckets THEN
          	bucket:= nb_buckets + 1;
   	ELSE
          	bucket := b;
   	END IF;
end;

