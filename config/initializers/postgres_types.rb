module ActiveRecord::ConnectionAdapters
  class PostgreSQLAdapter < AbstractAdapter
    # Monkey patched to remove varchar limit of 255 that Rails set, 
    # since it makes no performance difference in Postgres.
    def native_database_types 
      return {
        :primary_key => "serial primary key",
        :string      => { :name => "character varying" },
        :text        => { :name => "text" },
        :integer     => { :name => "integer" },
        :float       => { :name => "float" },
        :decimal     => { :name => "decimal" },
        :datetime    => { :name => "timestamp" },
        :timestamp   => { :name => "timestamp" },
        :time        => { :name => "time" },
        :date        => { :name => "date" },
        :binary      => { :name => "bytea" },
        :boolean     => { :name => "boolean" }
      }
    end
  end
end
