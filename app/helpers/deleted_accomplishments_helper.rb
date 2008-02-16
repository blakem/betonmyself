module DeletedAccomplishmentsHelper
  include BetHelper
  def render_descr_count(count)
    plural = count == 1 ? '' : 's'
    count == 0 ? '' : '<b>' + count.to_s + ' Deleted Accomplishment' + plural + '</b>'
  end
end
