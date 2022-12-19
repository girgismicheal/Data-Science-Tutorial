/****************************************************************************/
/*																			*/
/*	Kroenke, Auer, Vandenberg, and Yoder									*/
/*	Database Processing (15th Edition) Chapter 2							*/
/*																			*/
/*	MI Database: create table statements									*/
/*																			*/
/*	These are the MySQL 5.7 SQL code solutions								*/
/*	   for the Chapter 2 MI Database	                                    */
/****************************************************************************/
use mi_ch02;

CREATE TABLE ITEM (
		ItemID	            Int				    NOT NULL Identity(1, 1),
		Description		    VarChar(255)		NOT NULL,
		PurchaseDate		Date			NOT NULL,
		Store			    Char(50)		NOT NULL,
		City				Char(35)		NOT NULL,
		Quantity			Int		    	NOT NULL,
		LocalCurrencyAmount	Numeric(18,2)	NOT NULL,
		ExchangeRate	    Numeric(12,6)	NOT NULL,
		CONSTRAINT	Item_PK	   	 PRIMARY KEY (ItemID)
		);
        

CREATE TABLE SHIPMENT (
		ShipmentID			    Int				    NOT NULL Identity(1, 1),
		ShipperName		    	Char(35)		    NOT NULL,
		ShipperInvoiceNumber	Int				    NOT NULL,
		DepartureDate		    Date			    NULL,
		ArrivalDate				Date			    NULL,
		InsuredValue		    Numeric(12,2)		NULL,
		CONSTRAINT	Shipment_PK	  PRIMARY KEY (ShipmentID)
		);

 
CREATE TABLE SHIPMENT_ITEM (
		ShipmentID		   	Int				    NOT NULL,
		ShipmentItemID		Int				    NOT NULL,
		ItemID		    	Int				    NOT NULL,
		Value		  	    Numeric(12,2)		NOT NULL,
		CONSTRAINT	ShipmentItem_PK	    		PRIMARY KEY(ShipmentID, ShipmentItemID),
		CONSTRAINT	Ship_Item_Ship_FK		    FOREIGN KEY(ShipmentID)
                      REFERENCES SHIPMENT(ShipmentID)
                        ON DELETE CASCADE,
		CONSTRAINT	Ship_Item_Item_FK	FOREIGN KEY(ItemID)
                      REFERENCES ITEM(ItemID)
     		);
