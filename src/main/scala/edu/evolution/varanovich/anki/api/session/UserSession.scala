package edu.evolution.varanovich.anki.api.session

import edu.evolution.varanovich.anki.adt.Privileges
import edu.evolution.varanovich.anki.api.http.ServerConfig.loginAttempts

final case class UserSession(token: String, privileges: Privileges, userName: String, loginAttempts: Int = loginAttempts)