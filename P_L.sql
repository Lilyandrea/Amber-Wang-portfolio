

USE H_Accounting;

#P&L (income statement)
select distinct statement_section, statement_section_code from account as acc
inner join statement_section as state on state.statement_section_id=acc.profit_loss_section_id;
-- A stored procedure, or a stored routine, is like a function in other programming languages
-- We write the code once, and the code can de reused over and over again
-- We can pass on arguments into the stored procedure. i.e. we can give a specific 
-- input to a store procedure
-- For example we could determine the specific for which we want to produce the 
-- profit and loss statement

#  FIRST thing you MUST do whenever writting a stored procedure is to change the DELIMTER
#  The default deimiter in SQL is the semicolon ;
#  Since we will be using the semicolon to start and finish sentences inside the  stored procedure
#  The compiler of SQL won't know if the semicolon is closing the entire Stored Procedure or an line inside
#  Therefore, we change the DELIMITER so we can be explicit about whan we are 
# closing the stored procedure, vs. when
#  we are closing a specific Select  command
DROP PROCEDURE IF EXISTS H_Accounting.awang_sp;
-- The tpycal delimiter for Stored procedures is a double dollar sign
DELIMITER $$

CREATE PROCEDURE H_Accounting.awang_sp(varCalendarYear SMALLINT) #year
BEGIN
  
	-- We receive as an argument the year for which we will calculate the revenues
    -- This value is stored as an 'YEAR' type in the variable `varCalendarYear`
    -- To avoid confusion among which are fields from a table vs. which are the variables
    -- A good practice is to adopt a naming convention for all variables
    -- In these lines of code we are naming prefixing every variable as "var"
  
	-- We can define variables inside of our procedure
    -- declare many as variables as you want
	DECLARE varTotalRevenues DOUBLE DEFAULT 0;
    DECLARE varTotalCOGS DOUBLE DEFAULT 0;
     DECLARE varTotalSEXP DOUBLE DEFAULT 0;
    DECLARE varTotalOEXP DOUBLE DEFAULT 0;
    DECLARE varTotalOI DOUBLE DEFAULT 0;
    DECLARE varTotalINCTAX DOUBLE DEFAULT 0;
	--  We calculate the value of the sales for the given year and we store it into the variable we just declared
	SELECT 
    SUM(jeli.credit)
INTO varTotalRevenues FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'REV'
        AND YEAR(je.entry_date) = varCalendarYear
	;
 
SELECT 
    SUM(jeli.Credit)
INTO varTotalCOGS FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'COGS'
        AND YEAR(je.entry_date) = varCalendarYear
	;
    
    SELECT 
    SUM(jeli.credit)
INTO varTotalSEXP FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'SEXP'
        AND YEAR(je.entry_date) = varCalendarYear
	;
    
    SELECT 
    SUM(jeli.credit)
INTO varTotalOEXP FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'OEXP'
        AND YEAR(je.entry_date) = varCalendarYear
	;
    
SELECT 
    SUM(jeli.credit)
INTO varTotalOI FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'OI'
        AND YEAR(je.entry_date) = varCalendarYear
	;
    
    SELECT 
    SUM(jeli.credit)
INTO varTotalINCTAX FROM
    H_Accounting.journal_entry_line_item AS jeli
        INNER JOIN
    H_Accounting.account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    H_Accounting.journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
WHERE
    ss.statement_section_code = 'INCTAX'
        AND YEAR(je.entry_date) = varCalendarYear
	;
    
    -- Let's drop the `tmp` table where we will input the revenue
	-- The IF EXISTS is important. Because if the table does not exist the DROP alone would fail
	-- A store procedure will stop running whenever it faces an error. 
	DROP TABLE IF EXISTS H_Accounting.awang_tmp;
  
	-- Now we are certain that the table does not exist, we create with the columns that we need
	CREATE TABLE H_Accounting.awang_tmp (
    profit_loss_line_number INT,
    label VARCHAR(50),
    amount VARCHAR(50)
);
  
  -- Now we insert the a header for the report
  INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, Label, Amount)
	VALUES (1, 'PROFIT AND LOSS STATEMENT', "In '000s of USD");
  
	-- Next we insert an empty line to create some space between the header and the line items
	INSERT INTO H_Accounting.awang_tmp
				(profit_loss_line_number, label, amount)
		VALUES 	(2, '', '');
    
	-- Finally we insert the Total Revenues with its value
	INSERT INTO H_Accounting.awang_tmp
			(profit_loss_line_number, label, amount)
	VALUES 	(3, 'Total Revenues', format(varTotalRevenues / 1000, 2));
    
     INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(4, 'minus', '-');
    
    INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(5, 'COGS', format(varTotalCOGS / 1000, 2));
    
     INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(6, 'SEXP', format(varTotalSEXP / 1000, 2));
    
     INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(7, 'OEXP', format(varTotalOEXP / 1000, 2));
    
    INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(8, 'OI', format(varTotalOI / 1000, 2));
    
    INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(8, 'INCTAX', format(varTotalOI / 1000, 2));
    
    
    
      INSERT INTO H_Accounting.awang_tmp 
		   (profit_loss_line_number, label, amount)
	VALUES 	(9, 'Gross Profit', format((varTotalRevenues - varTotalCOGS) / 1000, 2));
END $$
DELIMITER ;
# THE LINE ABOVES CHANGES BACK OUR DELIMETER TO OUR USUAL ;


CALL H_Accounting.awang_sp(2019);

SELECT * FROM H_Accounting.awang_tmp;