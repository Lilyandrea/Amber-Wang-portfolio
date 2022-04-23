USE H_Accounting;
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
#  we are closing a specific Select command

#Balance Sheet column
select distinct statement_section, statement_section_code from account as acc
inner join statement_section state on state.statement_section_id=acc.balance_sheet_section_id;

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
	DECLARE varCurrentAssets DOUBLE DEFAULT 0;
    DECLARE varFixedAssets DOUBLE DEFAULT 0;
    DECLARE varDeferredAssets DOUBLE DEFAULT 0;
    DECLARE varCurrentLiabilties DOUBLE DEFAULT 0;
    DECLARE varEquity DOUBLE DEFAULT 0;
    
	--  We calculate the value of the sales for the given year and we store it into the variable we just declared
	SELECT SUM(jeli.debit)-SUM(jeli.credit) INTO varCurrentAssets
		FROM H_Accounting.journal_entry_line_item AS jeli
		INNER JOIN H_Accounting.account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN H_Accounting.journal_entry  AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
      
		WHERE ss.statement_section_code = "CA"
		AND YEAR(je.entry_date) = varCalendarYear
	;
 
        SELECT SUM(jeli.credit) INTO varFixedAssets
		FROM H_Accounting.journal_entry_line_item AS jeli
		INNER JOIN H_Accounting.account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN H_Accounting.journal_entry  AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
      
		WHERE ss.statement_section_code = "FA"
		AND YEAR(je.entry_date) = varCalendarYear
	;
    
     SELECT SUM(jeli.credit) INTO varDeferredAssets
		FROM H_Accounting.journal_entry_line_item AS jeli
		INNER JOIN H_Accounting.account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN H_Accounting.journal_entry  AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
      
      WHERE ss.statement_section_code = "DA"
		AND YEAR(je.entry_date) = varCalendarYear
	;
    
     SELECT SUM(jeli.credit)- SUM(jeli.debit) INTO varCurrentLiabilties
		FROM H_Accounting.journal_entry_line_item AS jeli
		INNER JOIN H_Accounting.account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN H_Accounting.journal_entry  AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
        
        WHERE ss.statement_section_code = "CL"
		AND YEAR(je.entry_date) = varCalendarYear
	;
   --  SELECT (ifnull(SUM(jeli.credit), 0) - ifnull(SUM(jeli.debit), 0)) INTO  varEquity--SUM(coalesce((jeli.credit, 0)) 
     SELECT SUM(coalesce(jeli.credit, 0)) - SUM(coalesce(jeli.debit, 0)) INTO varEquity
		FROM H_Accounting.journal_entry_line_item AS jeli
		INNER JOIN H_Accounting.account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN H_Accounting.journal_entry  AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN H_Accounting.statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
        
        WHERE ss.statement_section_code = "EQ"
		AND YEAR(je.entry_date) = varCalendarYear
	;
    
     
    -- Let's drop the `tmp` table where we will input the revenue
	-- The IF EXISTS is important. Because if the table does not exist the DROP alone would fail
	-- A store procedure will stop running whenever it faces an error. 
	DROP TABLE IF EXISTS H_Accounting.awang_tmp;
  
	-- Now we are certain that the table does not exist, we create with the columns that we need
	CREATE TABLE H_Accounting.awang_tmp
		(Assets VARCHAR(50), 
		 Equity VARCHAR(50), 
         Liabilities VARCHAR(50), 
	     amount VARCHAR(50)
		);
  
  -- Now we insert the a header for the report
  INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities ,amount)
	VALUES (1, 'Balance Sheet', "In '000s of USD","amount");
  
	-- Next we insert an empty line to create some space between the header and the line items
	INSERT INTO H_Accounting.awang_tmp
				(Assets, Equity, Liabilities ,amount)
		VALUES 	(2, '', '','');
    
	-- Finally we insert the Total Revenues with its value
	INSERT INTO H_Accounting.awang_tmp
			(Assets, Equity, Liabilities ,amount)
	VALUES 	(3, 'Current Assets', format(varCurrentAssets / 1000, 2),'');
    
     INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities ,amount)
	VALUES 	(4, 'plus', '+','');
    
    INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities, amount)
	VALUES 	(5, 'Fixed Assets', format(varFixedAssets / 1000, 2),'');
    
      INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities, amount)
	VALUES 	(5, 'Deferred Assets', format(varDeferredAssets / 1000, 2),'');
    
      INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities, amount)
	VALUES 	(5, 'CurrentLiabilties', format(varCurrentLiabilties / 1000, 2),'');
    
   INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities, amount)
	VALUES 	(5, 'varEquity', format(varEquity / 1000, 2),''); 
    
      INSERT INTO H_Accounting.awang_tmp 
		   (Assets, Equity, Liabilities ,amount)
	VALUES 	(6, 'Total Assets', format((varCurrentAssets + varFixedAssets) / 1000, 2),'');
END $$
DELIMITER ;
# THE LINE ABOVES CHANGES BACK OUR DELIMETER TO OUR USUAL ;


CALL H_Accounting.awang_sp(2018);

SELECT * FROM H_Accounting.awang_tmp;


