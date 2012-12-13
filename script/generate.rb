#!/usr/bin/env ruby

require "socket"

WORDS = %w{brood bucolic bungalow chatoyant conflate cynosure demure denouement desuetude elixir  ephemeral epiphany}
TYPES = %w{c g ms s}
HOST  = "127.0.0.1"
PORT  = 8125

module Math
  def self.max(a, b)
    a > b ? a : b
  end

  def self.min(a, b)
    a < b ? a : b
  end
end

def send(sock, val)
  sample = 1
  key  = WORDS.sample
  type = TYPES.sample
  rate = "|@#{sample}" unless sample == 1
  msg  = "#{key}:#{val}|#{type}#{rate}"
  puts msg
  sock.send(msg, 0, HOST, PORT)
end

numbers = Enumerator.new do |gen|
  val = 1
  i = 0
  loop do
    i = i + 0.2
    val = Math.max(-100, Math.min(1000, val + 0.8 * rand - 0.4 + 0.2 * Math.cos(i)))
    gen.yield val
  end
end

@sock = UDPSocket.new

while true do
  (1..10).to_a.sample.times do
    send(@sock, numbers.next())
  end

  sleep 0.5
end
