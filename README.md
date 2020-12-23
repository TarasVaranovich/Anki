#Anki
Course project for evolution scala bootcamp

## Required libraries
1. Unix-like system. Application tested on macOS Catalina 10.15.6
2. JRE 1.8 or above. Application tested with java 14 2020-03-17
3. Installed scala build tool. Application tested with sbt 1.3.13
4. Installed scala. Application tested with scala 2.13.3
5. Postgress 9.3 or above database. Application tested with version 13

## Application usage
1. Create database Anki and check database connection properties. 
2. Navigate to application sources root directory ~/Anki by terminal
3. Create and fill database tables: 
    sbt "runMain edu.evolution.varanovich.anki.utility.DatabaseUtility init"
3. Start server:
    sbt "runMain edu.evolution.varanovich.anki.api.http.AnkiServer"
4. Run client:
    sbt "runMain edu.evolution.varanovich.anki.client.AnkiClient"
5. Drop database tables: 
    sbt "runMain edu.evolution.varanovich.anki.utility.DatabaseUtility drop"