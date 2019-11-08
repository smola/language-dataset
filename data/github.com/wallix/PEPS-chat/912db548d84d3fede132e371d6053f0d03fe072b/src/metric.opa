/*
 * PEPS is a modern collaboration server
 * Copyright (C) 2015 MLstate
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



type Metric.id = Token.t

/** Full metric, with event, date and room. */
type Metric.t = {
  Metric.id id,
  Room.id room,
  Date.date date,
  metric metric
}

/** Partial metric, without the date information. */
type metric =
  {Metric.messageLog message} or
  {User.id connect} or // User connections.
  {User.id disconnect} or // User disconnections.
  {int cpu, int ram, int space} or
  {string badlogin, ip ip}

type Metric.messageLog = {
  User.id user, string text,
  ip ip, string userAgent
}

/** Database declaration. */
database opametric {
  Metric.t /metrics[{id}]
}
database /opametric/metrics[_]/metric full


module Metric {

  /** Generic logger. */
  protected function log(Room.id room, metric metric) {
    fullmetric = ~{id: Token.generate(), room, date: Date.now(), metric}
    /opametric/metrics[id == fullmetric.id] <- fullmetric
  }

  /** Log message broadcasts. */
  protected function logMessage(User.id user, Room.id room, string text) {
    ip = HttpRequest.get_ip() ? 0.0.0.0
    userAgent = match (HttpRequest.get_user_agent()) {
      case {some: userAgent}: "{userAgent}"
      case {none}: "unknown"
    }
    message = ~{user, text, userAgent, ip}
    log(room, ~{message})
  }

  /** Log user connections. */
  protected @expand function logConnect(User.id user, Room.id room) {
    log(room, {connect: user})
  }

  /** Log user disconnections. */
  protected @expand function logDisconnect(User.id user, Room.id room) {
    log(room, {disconnect: user})
  }

  /** Log server statistics. */
  protected @expand function logStatistics(Room.id room, int cpu, int ram, int space) {
    log(room, ~{cpu, ram, space})
  }

  /** Log failed login attempts. */
  protected @expand function logBadlogin(Room.id room, string badlogin, ip ip) {
    log(room, ~{badlogin, ip})
  }

  date_format = "%Y%m%d%H%M%S%x"
  date_scanner = Date.generate_scanner(date_format)

  /** Fetch metrics from the database. */
  function list(Metric.t) fetch(Room.id room, int limit, int skip, option(string) timestamp) {
    (match (timestamp) {
      case {none}:
        DbSet.iterator(/opametric/metrics[
          room == room; limit limit;
          skip skip; order -date
        ])
      case ~{some}:
        date = Date.of_formatted_string(date_scanner, some)
        match (date) {
          case {some: date}:
            DbSet.iterator(/opametric/metrics[
              room == room and date >= date;
              order -date
            ])
          default:
            Log.notice("error", "timestamp invalid")
            Iter.empty
        }
    }) |> Iter.to_list
  }

  /** Transform a metric into JSON format. */
  function RPC.Json.json toJson(Metric.t metric) {
    {Record: [
      ("room", {String: metric.room}),
      ("date", {Int: Date.in_milliseconds(metric.date)}),
      ("metric",
        {Record:
          match (metric.metric) {
            case {message: ~{user, text, ip, userAgent ...}}:
              [ ("user", {String: user}),
                ("textlen", {Int: String.length(text)}),
                ("ip", {String: "{ip}"}),
                ("userAgent", {String: userAgent}) ]
            case {connect: user}:
              [ ("connect", {String: user}) ]
            case {disconnect: user}:
              [ ("disconnect", {String: user}) ]
            case ~{cpu, ram, space}:
              [ ("cpu", {Int: cpu}),
                ("ram", {Int: ram}),
                ("space", {Int: space}) ]
            case ~{badlogin, ip}:
              [ ("badlogin", {String: badlogin}),
                ("ip", {String: "{ip}"}) ]
            default:
              [ ("error", {String: "Unsupported metric type"}) ]
          }})
    ]}
  }

} // END METRIC
