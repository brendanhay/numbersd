#!/usr/bin/env ruby

require "socket"

WORDS = %w{bucolic bungalow conflate cynosure denouement desuetude ephemeral epiphany}
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
  key  = WORDS.sample
  type = TYPES.sample
  samp = [0.1, 0.5, nil].sample
  rate = "|@#{samp}" if samp
  msg  = "#{key}:#{val}|#{type}#{rate}"
  puts msg
  sock.send(msg, 0, HOST, PORT)
end

numbers = Enumerator.new do |gen|
  val = 1
  i = 0
  loop do
    i = i + 0.2
    val = Math.max(0, Math.min(1000, val + 0.8 * rand - 0.4 + 0.2 * Math.cos(i)))
    gen.yield val
  end
end

@sock = UDPSocket.new

SAMPLE = []

while true do
  (1..10).to_a.sample.times do
    send(@sock, numbers.next())
  end

  sleep 1
end
