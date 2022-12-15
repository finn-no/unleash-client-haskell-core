#! /usr/bin/env nix-shell
#! nix-shell -p curl cacert ruby -i ruby --pure

require 'time'

CHANNEL = "nixos-22.05"
AT_LEAST_DAYS_OLD = 21
HASH_FILE = "nixpkgs.hash"

threshold = (DateTime.now - AT_LEAST_DAYS_OLD).to_time.to_i

# Download history
history_lines = `curl -s https://channels.nix.gsc.io/#{CHANNEL}/history`.split("\n")

class Release
  attr_reader :sha, :epoch
  def initialize(s)
    @sha, epoch_str = s.split(" ")
    @epoch = epoch_str.to_i
  end
  def to_s
    [@sha, @epoch].to_s
  end
end

release = history_lines
       .map { |line| Release.new(line) }
       .sort_by { |t|  t.epoch }
       .reverse
       .find { |t| t.epoch < threshold }

puts "Picked first release older than #{AT_LEAST_DAYS_OLD} days: #{release.sha} (#{Time.at(release.epoch)})"

File.open(HASH_FILE, "w") do |file|
  file.print release.sha
end
