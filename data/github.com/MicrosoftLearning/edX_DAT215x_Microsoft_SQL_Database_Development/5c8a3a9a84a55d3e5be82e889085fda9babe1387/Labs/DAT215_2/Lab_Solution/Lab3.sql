--Challenge 1
--Create dbo.FormatPhoneNumber Function

CREATE FUNCTION dbo.FormatPhoneNumber
( @PhoneNumberToFormat nvarchar(16)
)
RETURNS nvarchar(16)
AS BEGIN
DECLARE @Digits nvarchar(16) = '';
DECLARE @Remaining nvarchar(16) = @PhoneNumberToFormat;
DECLARE @Character nchar(1);
IF LEFT(@Remaining,1) = N'+' RETURN @Remaining;
WHILE (LEN(@Remaining) > 0) BEGIN
SET @Character = LEFT(@Remaining,1);
SET @Remaining = SUBSTRING(@Remaining,2,LEN(@Remaining));
IF (@Character >= N'0') AND (@Character <= N'9')
SET @Digits += @Character;
END;
RETURN CASE LEN(@Digits)
WHEN 10 THEN N'(' + SUBSTRING(@Digits,1,3) + N') '
+ SUBSTRING(@Digits,4,3) + N'-'
+ SUBSTRING(@Digits,7,4)
WHEN 8 THEN SUBSTRING(@Digits,1,4) + N'-'
+ SUBSTRING(@Digits,5,4)
WHEN 6 THEN SUBSTRING(@Digits,1,3) + N'-'
+ SUBSTRING(@Digits,4,3)
ELSE @Digits
END;
END;
GO


--Test the Function

SELECT dbo.FormatPhoneNumber('+61 3 9485-2342');
SELECT dbo.FormatPhoneNumber('415 485-2342');
SELECT dbo.FormatPhoneNumber('(41) 5485-2342');
SELECT dbo.FormatPhoneNumber('94852342');
SELECT dbo.FormatPhoneNumber('85-2342');
GO
