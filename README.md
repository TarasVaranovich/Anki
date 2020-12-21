# Anki
Course project for evolution scala bootcamp

Create tables and fill them from resources:
sbt "runMain edu.evolution.varanovich.anki.utility.DatabaseUtility init"

Drop tables:
sbt "runMain edu.evolution.varanovich.anki.utility.DatabaseUtility drop"

Start server:
sbt "runMain edu.evolution.varanovich.anki.api.http.AnkiServer"

Run client:
sbt "runMain edu.evolution.varanovich.anki.client.AnkiClient"