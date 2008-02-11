class CreatePseudos < ActiveRecord::Migration
  def self.up
    create_table :pseudos do |t|
      t.column :type, :string
    end
  end
  def self.down
    drop_table :pseudos
  end
end
