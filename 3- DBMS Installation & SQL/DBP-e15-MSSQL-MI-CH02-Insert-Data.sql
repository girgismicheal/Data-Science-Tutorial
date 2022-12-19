/****************************************************************************/
/*																			*/
/*	Kroenke, Auer, Vandenberg, and Yoder									*/
/*	Database Processing (15th Edition) Chapter 2							*/
/*																			*/
/*	MI Database: INSERT statements											*/
/*																			*/
/*	These are the MySQL 5.7 SQL code solutions								*/
/*	   for the Chapter 2 MI Database	                                    */
/****************************************************************************/

 
/*****   ITEM Data   *************************************************/

INSERT INTO ITEM VALUES(
		'QE Dining set', '2018-04-07', 
		 'Eastern Treasures', 'Manila', 2, 403405, 0.01774);
INSERT INTO ITEM VALUES(
		'Willow Serving Dishes', '2018-07-15', 
		 'Jade Antiques', 'Singapore', 75, 102, 0.5903);
INSERT INTO ITEM VALUES(
		'Large Bureau', '2018-07-17', 
		 'Eastern Sales', 'Singapore', 8, 2000, 0.5903);
INSERT INTO ITEM VALUES(
		'Brass Lamps', '2018-07-20', 
		 'Jade Antiques', 'Singapore', 40, 50, 0.5903);

 

/*****   SHIPMENT Data   ***************************************************/

INSERT INTO SHIPMENT VALUES(
		'ABC Trans-Oceanic', 2017651, '2017-12-10', '2018-03-15', 15000.00);
INSERT INTO SHIPMENT VALUES(
		'ABC Trans-Oceanic', 2018012, '2018-01-10', '2018-03-20', 12000.00);
INSERT INTO SHIPMENT VALUES(
		'Worldwide', 49100300, '2018-05-05', '2018-06-17', 20000.00);
INSERT INTO SHIPMENT VALUES(
		'International', 399400, '2018-06-02', '2018-07-17', 17500.00 );
INSERT INTO SHIPMENT VALUES(
		'Worldwide', 84899440, '2018-07-10', '2018-07-28', 25000.00);
INSERT INTO SHIPMENT VALUES(
		'International', 488955, '2018-08-05', '2018-09-11', 18000.00);



/*****   SHIPMENT_ITEM Data   **********************************************/

INSERT INTO SHIPMENT_ITEM VALUES(3, 1, 1, 15000);
INSERT INTO SHIPMENT_ITEM VALUES(4, 1, 4, 1200);
INSERT INTO SHIPMENT_ITEM VALUES(4, 2, 3, 9500);
INSERT INTO SHIPMENT_ITEM VALUES(4, 3, 2, 4500);

