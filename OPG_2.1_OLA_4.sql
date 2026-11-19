-- 1. Invariant tabel (Stamdata)
CREATE TABLE Car_Details (
    carid VARCHAR(50) PRIMARY KEY,
    Mærke VARCHAR(100),
    Model VARCHAR(100),
    Registrering_Måned INT,
    Registrering_År INT,
    Kilometer_kørt INT, 
    Forhandler VARCHAR(255),
    Link VARCHAR(500)
);

-- 2. Tidsserie-tabel (Priser og status)
CREATE TABLE Car_Pricing (
    price_id INT AUTO_INCREMENT PRIMARY KEY,
    carid VARCHAR(50) NOT NULL,
    Pris_DKK DECIMAL(12,2),
    scrapedate DATE NOT NULL,
    sold BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (carid) REFERENCES Car_Details(carid)
);