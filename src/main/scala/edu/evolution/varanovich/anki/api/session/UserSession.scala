package edu.evolution.varanovich.anki.api.session

import edu.evolution.varanovich.anki.config.ServerConfig
import edu.evolution.varanovich.anki.model.Privileges

final case class UserSession(token: String,
                             privileges: Privileges,
                             userName: String,
                             loginAttempts: Int = ServerConfig.load.loginAttempts,
                             keyAliasMap: Map[String, Int] = Map())