class PriorityQueue
  define_tables do |tables|
    tables.add_table :items, UnorderedTable(name, price)
  end

  def insert(name, price) 
    items.insert(name, price)
  end

  def delete(name)
    items.where(:name => name).delete_all
  end

  def min_price
    items.minimum(:price)
  end
end


class PriorityQueue
  define_tables do |tables|
    tables.add_table :items, UnorderedTable(name, us_price)
  end

  def insert(name, price)
    items.where(:name => name).delete_all
    items.insert(name, us_price)
  end

  def delete(name)
    items.where(:name => name).delete_all
  end
end

class InsanePriorityQueue
  define_tables do |tables|
    tables.add_table :items, UnorderedTable(name, us_price, aud_price)
  end

  def insert(name, us_price, aud_price)
    items.where(:name => name).delete_all
    items.insert(name, us_price, aud_price)
  end

  def delete(name)
    items.where(:name => name).delete_all
  end

  def get_cheapest(exchange_rate)
    items.minimum_by({ |item| item.us_price + item.aud_price * exchange_rate})
  end
end

