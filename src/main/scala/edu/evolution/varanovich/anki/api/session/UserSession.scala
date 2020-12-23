package edu.evolution.varanovich.anki.api.session

import edu.evolution.varanovich.anki.api.http.ServerConfig.loginAttempts
import edu.evolution.varanovich.anki.model.Privileges

final case class UserSession(token: String,
                             privileges: Privileges,
                             userName: String,
                             loginAttempts: Int = loginAttempts,
                             keyAliasMap: Map[String, Int] = Map())