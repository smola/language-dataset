
#dataset structure       
Column 1 : PassengerId
Column 2 : Survived  (survived=0 & died=1)
Column 3 : Pclass
Column 4 : Name
Column 5 : Sex
Column 6 : Age
Column 7 : SibSp
Column 8 : Parch
Column 9 : Ticket
Column 10 : Fare
Column 11 : Cabin
Column 12 : Embarked
        
		

create database titanic


--Load the data into the table titanicdata
--Now the data should be loaded into titanicdata table, we can start analyzing


--Number of males and females
select sex, count(sex) as number from titanicdata group by sex


--Number of dead and live
select (CASE Survived 
		WHEN 0 THEN 'survived'
		WHEN 1 THEN 'dead'
		END) as status, count(Survived) as number from titanicdata group by Survived


--Number of dead and live by sex
select sex, (CASE Survived
				WHEN 0 THEN 'Survived'
				WHEN 1 THEN 'dead'
				END) as status, count(*) number from titanicdata group by sex, Survived


--Number of people in each class
select Pclass , count(*) as number from titanicdata group by (Pclass)


--Number of dead and live in each class
select Pclass, (CASE Survived
				WHEN 0 THEN 'Survived'
				WHEN 1 THEN 'dead'
				END) as status, count(*) as number from titanicdata group by (Pclass,Survived)


--Number of dead and live by sex in each class
select (CASE Survived
				WHEN 0 THEN 'Survived'
				WHEN 1 THEN 'dead'
				END) as status, sex, Pclass from titanicdata group by Survived, sex, Pclass
